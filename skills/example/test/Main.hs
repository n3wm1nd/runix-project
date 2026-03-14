{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Polysemy
import Polysemy.Fail (runFail)
import Polysemy.State (State, runState)
import qualified Data.ByteString.Lazy as BSL

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic (..))
import qualified UniversalLLM.Providers.Anthropic as Provider

import Runix.LLM (LLM)
import Runix.LLM.Interpreter
  ( interpretLLMStreamingWith
  , AnthropicOAuthAuth (..)
  , EnableStreaming
  , ProtocolRequest
  )
import Runix.LLMStream (LLMStreaming)
import Runix.Logging (Logging, loggingNull)
import Runix.Cancellation (cancelNoop)
import Runix.StreamChunk (ignoreChunks)
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.Streaming (interpretStreamingStateful)
import qualified Runix.HTTP as HTTPEff
import Runix.HTTP (HTTPResponse (..))
import Runix.Streaming (HTTPStreamResult (..))

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM

import Runix.Skill.Testing (SkillTest (..), TestOracle (..), runSkillTest)
import Runix.Skill.CacheLLM (CachePath (..), cacheLLM)
import Skill.Example (exampleSkill)

import qualified Data.ByteString as BS
import Autodocodec (HasCodec)
import Data.Default (Default)
import UniversalLLM.Protocols.Anthropic (AnthropicResponse)

--------------------------------------------------------------------------------
-- Test model
--------------------------------------------------------------------------------

data TestM = TestM deriving stock (Show, Eq)

instance ModelName (Model TestM Anthropic) where
  modelName _ = "claude-haiku-4-5-20251001"

instance HasTools (Model TestM Anthropic) where
  withTools = Provider.anthropicTools

type TestModel = Model TestM Anthropic

testProvider
  :: ComposableProvider TestModel (ToolState TestModel, ())
testProvider = withTools `chainProviders` Provider.baseComposableProvider

--------------------------------------------------------------------------------
-- SSE fixture: assistant calls echo, then gives a text response
--
-- In a real test this comes from a recorded cache file.  Here we inline a
-- minimal fixture so the test has no external dependencies.
--------------------------------------------------------------------------------

-- | A minimal SSE stream where the model calls the echo tool once then stops.
echoCallFixture :: BSL.ByteString
echoCallFixture = BSL.fromChunks
  -- tool_use block
  [ "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_01\",\"name\":\"echo\",\"input\":{}}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"input\\\":\"}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"hello\\\"\"}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"}\"}}\n\n"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":10}}\n\n"
  , "data: {\"type\":\"message_stop\"}\n\n"
  ]

-- | A minimal SSE stream where the model gives a plain text response.
echoTextFixture :: BSL.ByteString
echoTextFixture = BSL.fromChunks
  [ "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hello\"}}\n\n"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}\n\n"
  , "data: {\"type\":\"message_stop\"}\n\n"
  ]

--------------------------------------------------------------------------------
-- Mock HTTP + runner
--------------------------------------------------------------------------------

-- | Mock HTTP that serves a sequence of SSE fixtures round-robin.
-- The first call gets fixture[0], the second gets fixture[1], etc.
mockHTTPSequence
  :: forall r a.
     Members '[Logging, Embed IO, Fail] r
  => [BSL.ByteString]
  -> Sem (HTTP ': HTTPStreaming ': State Int ': r) a
  -> Sem r a
mockHTTPSequence fixtures action = do
  evalState (0 :: Int) $
    interpretStreamingStateful onStart onFetch onClose $
    interpret @HTTP (\case
      HTTPEff.HttpRequest _ ->
        return $ Right $ HTTPResponse 200
          [("content-type", "application/json")]
          "{\"id\":\"mock\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"mock\",\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}")
    action
  where
    currentFixture :: Sem (State Int ': r) BSL.ByteString
    currentFixture = do
      idx <- get @Int
      modify @Int (+1)
      return $ if idx < length fixtures then fixtures !! idx else BSL.empty

    onStart :: HTTPRequest -> Sem (State Int ': r) (Either String [BS.ByteString])
    onStart _ = do
      body <- currentFixture
      return $ Right (BSL.toChunks body)

    onFetch :: [BS.ByteString] -> Sem (State Int ': r) (Maybe BS.ByteString, [BS.ByteString])
    onFetch []     = return (Nothing, [])
    onFetch (c:cs) = return (Just c, cs)

    onClose :: [BS.ByteString] -> Sem (State Int ': r) HTTPStreamResult
    onClose _ = return $ HTTPStreamResult 200 [] BSL.empty

-- | Test runner for the example skill.
-- Supplies mocked HTTP serving two fixtures: first the tool call, then
-- the follow-up text response after the tool result is returned.
testRunner
  :: [BSL.ByteString]
  -> Sem '[ LLM TestModel
           , Fail
           , Logging
           , State [Message TestModel]
           ] a
  -> IO (Either String a, [Message TestModel])
testRunner fixtures action =
  fmap reorder $
  runM
  . runState @[Message TestModel] []
  . loggingNull
  . runFail
  . mockHTTPSequence fixtures
  . cancelNoop
  . ignoreChunks @BS.ByteString
  . interpretLLMStreamingWith (AnthropicOAuthAuth "mock") testProvider TestModel []
  . cacheLLM (CachePath "test-fixtures")
  $ action
  where
    reorder ((err, history), ()) = (err, history)

--------------------------------------------------------------------------------
-- Skill tests
--------------------------------------------------------------------------------

echoSkillTests :: [SkillTest TestModel]
echoSkillTests =
  [ SkillTest
      { testName   = "calls echo tool when asked to echo"
      , testPrompt = "Please echo the word 'hello'"
      , testOracle = ToolWasCalled "echo"
      }
  , SkillTest
      { testName   = "echo tool is called with the right input"
      , testPrompt = "Echo exactly: hello"
      , testOracle = ToolCalledWith "echo" $ \args ->
          case args of
            Object o -> case KM.lookup "input" o of
              Just (String t) -> t == "hello"
              _               -> False
            _ -> False
      }
  ]

main :: IO ()
main = hspec $ describe "Example Skill" $
  mapM_ (runSkillTest (testRunner [echoCallFixture, echoTextFixture]) exampleSkill)
    echoSkillTests
