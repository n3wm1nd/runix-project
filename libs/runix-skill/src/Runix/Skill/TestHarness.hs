{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
-- | Shared test harness for skill test suites.
--
-- Provides:
--
--  * 'mockHTTPSequence' — serve a list of SSE fixtures round-robin,
--    duplicated no more across skill test suites.
--
--  * 'SkillEnv' — environment discovery for integration tests: reads
--    @TEST_MODE@ and provider credentials from the environment.
--
--  * 'integrationSkip' — mark a test as pending when no live LLM is
--    available, following the same pattern as 'TestCache.playbackMode'.
module Runix.Skill.TestHarness
  ( -- * Mocked HTTP (deterministic tests)
    mockHTTPSequence
    -- * Integration test environment
  , SkillEnv (..)
  , TestMode (..)
  , loadSkillEnv
  , integrationSkip
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Environment (lookupEnv)

import Polysemy (Sem, Members, Member, interpret, embed)
import Polysemy.Fail (Fail)
import Polysemy.State (State, evalState, get, modify)
import Test.Hspec (pendingWith)

import Runix.HTTP (HTTP, HTTPStreaming, HTTPResponse (..))
import qualified Runix.HTTP as HTTPEff
import Runix.Streaming (interpretStreamingStateful, HTTPStreamResult (..))
import Runix.Logging (Logging)

--------------------------------------------------------------------------------
-- Mocked HTTP
--------------------------------------------------------------------------------

-- | Interpret HTTP + HTTPStreaming by serving a list of SSE fixtures in order.
-- Each LLM call consumes the next fixture; extras return an empty stream.
-- Eliminates duplication across skill test suites.
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
        "{\"id\":\"mock\",\"type\":\"message\",\"role\":\"assistant\",\
        \\"content\":[],\"model\":\"mock\",\"stop_reason\":\"end_turn\",\
        \\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}")
  action
  where
    nextFixture :: Sem (State Int ': r) BSL.ByteString
    nextFixture = do
      idx <- get @Int
      modify @Int (+ 1)
      return $ if idx < length fixtures then fixtures !! idx else BSL.empty

    onStart _ = do
      body <- nextFixture
      return $ Right (BSL.toChunks body)

    onFetch []     = return (Nothing, [])
    onFetch (c:cs) = return (Just c, cs)

    onClose _ = return $ HTTPStreamResult 200 [] BSL.empty

--------------------------------------------------------------------------------
-- Integration test environment
--------------------------------------------------------------------------------

-- | How the integration tests should run.
data TestMode
  = Playback   -- ^ Use cached responses only (default, no credentials needed).
  | Record     -- ^ Use live API; cache new responses.
  | Update     -- ^ Use live API; overwrite existing cached responses.
  | Live       -- ^ Use live API; ignore cache entirely.
  deriving (Show, Eq)

-- | Environment for skill integration tests.
-- Credentials are 'Nothing' when the relevant env var is absent.
data SkillEnv = SkillEnv
  { skillTestMode         :: TestMode
  , skillOpenRouterKey    :: Maybe Text
  , skillAnthropicToken   :: Maybe Text
  , skillLlamaCppEndpoint :: Maybe String
  , skillCachePath        :: FilePath
  }

-- | Read the skill test environment from environment variables.
--
-- Relevant variables:
--   @TEST_MODE@              — @"record"@, @"update"@, @"live"@, or absent (playback)
--   @OPENROUTER_API_KEY@     — OpenRouter API key
--   @ANTHROPIC_OAUTH_TOKEN@  — Anthropic OAuth token
--   @LLAMACPP_ENDPOINT@      — llama.cpp server URL
loadSkillEnv :: FilePath -> IO SkillEnv
loadSkillEnv cachePath = do
  modeStr         <- lookupEnv "TEST_MODE"
  openRouterKey   <- fmap (fmap T.pack) $ lookupEnv "OPENROUTER_API_KEY"
  anthropicToken  <- fmap (fmap T.pack) $ lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  llamaCppEp      <- lookupEnv "LLAMACPP_ENDPOINT"
  let mode = case modeStr of
        Just "record" -> Record
        Just "update" -> Update
        Just "live"   -> Live
        _             -> Playback
  return SkillEnv
    { skillTestMode         = mode
    , skillOpenRouterKey    = openRouterKey
    , skillAnthropicToken   = anthropicToken
    , skillLlamaCppEndpoint = llamaCppEp
    , skillCachePath        = cachePath
    }

-- | Mark the current test as pending when a live LLM is required but no
-- credentials are present.
--
-- Integration tests run in 'Playback' mode by default — they replay from
-- cache and pass/fail without any external interaction.  Call this only for
-- tests that unconditionally need live API access (e.g. 'Live' mode benchmarks).
-- For normal cache-backed integration tests, just let 'cacheLLM' handle it;
-- if a cache file is missing the test will be marked pending automatically.
integrationSkip :: SkillEnv -> String -> IO ()
integrationSkip env reason =
  when (skillTestMode env == Live && noCredentials env) $
    pendingWith $ "Integration test skipped (no credentials). " <> reason
                <> " Provide API credentials to run in live mode."
  where
    noCredentials e =
      skillOpenRouterKey e == Nothing &&
      skillAnthropicToken e == Nothing &&
      skillLlamaCppEndpoint e == Nothing
