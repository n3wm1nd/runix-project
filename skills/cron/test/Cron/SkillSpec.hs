{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Skill-level tests: agent loop + oracle assertions.
--
-- Uses the in-memory 'Cron' interpreter and a mocked LLM (SSE fixtures).
-- The agent loop runs fully; we assert on what tools it called.
module Cron.SkillSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Fail (runFail)
import Polysemy.State (State, runState, evalState)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic (..))
import qualified UniversalLLM.Providers.Anthropic as Provider
import UniversalLLM.Protocols.Anthropic (AnthropicResponse)

import Runix.LLM (LLM)
import Runix.LLM.Interpreter
  ( interpretLLMStreamingWith
  , AnthropicOAuthAuth (..)
  )
import Runix.LLMStream (LLMStreaming)
import Runix.Logging (Logging, loggingNull)
import Runix.Cancellation (cancelNoop)
import Runix.StreamChunk (ignoreChunks)
import Runix.HTTP (HTTP, HTTPStreaming, HTTPResponse (..))
import Runix.Streaming (interpretStreamingStateful, HTTPStreamResult (..))
import qualified Runix.HTTP as HTTPEff

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM

import Runix.Skill.Testing (SkillTest (..), TestOracle (..), runSkillTest)
import Runix.Skill.CacheLLM (CachePath (..), cacheLLM)
import Cron.Effect (Cron, CrontabText (..))
import Cron.InMemory (cronInMemory)
import Cron.Skill (cronSkill)

--------------------------------------------------------------------------------
-- Test model (same pattern as runix/test/Main.hs)
--------------------------------------------------------------------------------

data TestM = TestM deriving stock (Show, Eq)

instance ModelName (Model TestM Anthropic) where
  modelName _ = "claude-haiku-4-5-20251001"

instance HasTools (Model TestM Anthropic) where
  withTools = Provider.anthropicTools

type TestModel = Model TestM Anthropic

testProvider :: ComposableProvider TestModel (ToolState TestModel, ())
testProvider = withTools `chainProviders` Provider.baseComposableProvider

--------------------------------------------------------------------------------
-- SSE fixtures
--
-- Each fixture is a minimal SSE stream that makes the agent call one specific
-- tool then stop.  Recorded from real API calls; inlined here for portability.
--------------------------------------------------------------------------------

-- | Agent calls cron_list then gives a text response.
cronListFixture :: BSL.ByteString
cronListFixture = BSL.fromChunks
  [ "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"t01\",\"name\":\"cron_list\",\"input\":{}}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{}\"}}\n\n"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":5}}\n\n"
  , "data: {\"type\":\"message_stop\"}\n\n"
  ]

-- | Agent calls cron_add then gives a text response.
cronAddFixture :: BSL.ByteString
cronAddFixture = BSL.fromChunks
  [ "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"t02\",\"name\":\"cron_add\",\"input\":{}}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"schedule\\\":\"}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"0 * * * *\\\",\"}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"command\\\":\"}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"backup.sh\\\"}\"}}\n\n"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":10}}\n\n"
  , "data: {\"type\":\"message_stop\"}\n\n"
  ]

-- | Agent removes a cron job.
cronRemoveFixture :: BSL.ByteString
cronRemoveFixture = BSL.fromChunks
  [ "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"t03\",\"name\":\"cron_remove\",\"input\":{}}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"pattern\\\":\\\"backup\\\"}\"}}\n\n"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":8}}\n\n"
  , "data: {\"type\":\"message_stop\"}\n\n"
  ]

-- | Plain text response to end the agent loop.
textResponseFixture :: BSL.ByteString
textResponseFixture = BSL.fromChunks
  [ "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Done.\"}}\n\n"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}\n\n"
  , "data: {\"type\":\"message_stop\"}\n\n"
  ]

--------------------------------------------------------------------------------
-- Mock HTTP serving a fixture sequence
--------------------------------------------------------------------------------

mockHTTPSequence
  :: forall r a.
     Members '[Logging, Embed IO, Fail] r
  => [BSL.ByteString]
  -> Sem (HTTP ': HTTPStreaming ': State Int ': r) a
  -> Sem r a
mockHTTPSequence fixtures action =
  evalState (0 :: Int) $
  interpretStreamingStateful onStart onFetch onClose $
  interpret @HTTP (\case
    HTTPEff.HttpRequest _ ->
      return $ Right $ HTTPResponse 200
        [("content-type", "application/json")]
        "{\"id\":\"m\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"mock\",\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}")
  action
  where
    currentFixture :: Sem (State Int ': r) BSL.ByteString
    currentFixture = do
      idx <- get @Int
      modify @Int (+1)
      return $ if idx < length fixtures then fixtures !! idx else BSL.empty

    onStart _ = do
      body <- currentFixture
      return $ Right (BSL.toChunks body)

    onFetch []     = return (Nothing, [])
    onFetch (c:cs) = return (Just c, cs)

    onClose _ = return $ HTTPStreamResult 200 [] BSL.empty

--------------------------------------------------------------------------------
-- Test runner
--------------------------------------------------------------------------------

-- | Run a skill test with an in-memory cron and mocked LLM.
-- The initial crontab content is passed as @CrontabText@.
testRunner
  :: CrontabText
  -> [BSL.ByteString]
  -> Sem '[ LLM TestModel
           , Fail
           , Logging
           , State [Message TestModel]
           , Cron
           , State CrontabText
           ] a
  -> IO (Either String a, [Message TestModel])
testRunner initialCron fixtures action =
  fmap reorder $
  runM
  . evalState initialCron
  . cronInMemory
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
    reorder ((err, history), _cronState) = (err, history)

--------------------------------------------------------------------------------
-- Skill tests
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Cron Skill" $ do
  mapM_ runTest cronSkillTests

  where
    runTest t =
      runSkillTest
        (testRunner (CrontabText "0 * * * * /bin/backup\n") [fst (testFixtures t), textResponseFixture])
        (cronSkill @'[Cron, State CrontabText])
        t

    testFixtures t = case testName t of
      "lists cron jobs when asked" -> (cronListFixture, ())
      "adds a cron job when asked" -> (cronAddFixture, ())
      "removes a cron job when asked" -> (cronRemoveFixture, ())
      _ -> (textResponseFixture, ())

cronSkillTests :: [SkillTest TestModel]
cronSkillTests =
  [ SkillTest
      { testName   = "lists cron jobs when asked"
      , testPrompt = "Show me my current cron jobs"
      , testOracle = ToolWasCalled "cron_list"
      }
  , SkillTest
      { testName   = "adds a cron job when asked"
      , testPrompt = "Add a cron job that runs backup.sh every hour"
      , testOracle = ToolWasCalled "cron_add"
      }
  , SkillTest
      { testName   = "removes a cron job when asked"
      , testPrompt = "Remove the backup cron job"
      , testOracle = OracleAll
          [ ToolWasCalled "cron_remove"
          , ToolCalledWith "cron_remove" $ \args ->
              case args of
                Object o -> case KM.lookup "pattern" o of
                  Just (String p) -> "backup" `T.isInfixOf` p
                  _ -> False
                _ -> False
          ]
      }
  ]
