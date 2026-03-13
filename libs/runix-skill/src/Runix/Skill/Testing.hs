-- | Skill test infrastructure.
--
-- Provides the standard building blocks for skill test suites:
--
--  * 'TestModel' — a pre-wired Anthropic model type; use @\@TestModel@ in tests
--    instead of declaring your own model type and instances.
--
--  * 'run' — root runner: interprets 'TestEnvironment' and 'Fail' into hspec
--    outcomes (@IO ()@).  All IO is mediated through the effect stack.
--
--  * 'interpretLLM' — LLM interpreter for integration tests; uses cached
--    responses in playback mode, live API in record\/live mode.
--
--  * 'interpretLLMMocked' — LLM interpreter for deterministic tests; serves
--    preloaded SSE fixtures in order, no network access required.
--
--  * 'toolCallFixture' \/ 'textResponseFixture' — minimal SSE byte sequences
--    for constructing mocked LLM responses.
--
--  * 'assert' — assertion helper that signals failure via 'Fail'.
--
--  * 'toolWasCalled' \/ 'toolCalledWith' — predicates over the message history.
--
-- Typical usage:
--
-- @
-- -- integration spec
-- run = Testing.run . Testing.interpretLLM
-- runWithCrons initial = run . evalState initial . cronInMemory
--
-- it "calls cron_list" $
--   runWithCrons (CrontabText "…") $ do
--     history <- runSkill \@TestModel "Show me my cron jobs" cronSkill
--     assert "cron_list was called" (toolWasCalled "cron_list" history)
-- @
module Runix.Skill.Testing
  ( -- * Canonical test model
    TestModel
    -- * Root runner
  , run
    -- * Test environment effect
  , TestEnvironment
    -- * LLM interpreters
  , interpretLLM
  , interpretLLMMocked
    -- * SSE fixture helpers
  , SSEFixture
  , toolCallFixture
  , textResponseFixture
    -- * Assertion and predicate helpers
  , assert
  , toolWasCalled
  , toolCalledWith
  ) where

import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (Value)
import Data.Kind (Type)
import Polysemy (Sem, Embed, Member)
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import Runix.Logging (Logging)
import UniversalLLM (Message (..), ToolCall (..), Model, ModelName (..), HasTools (..))
import UniversalLLM.Providers.Anthropic (Anthropic (..), anthropicTools)
import Runix.LLM (LLM)

--------------------------------------------------------------------------------
-- Canonical test model
--------------------------------------------------------------------------------

-- | Opaque tag for the default test model.
data TestM = TestM deriving stock (Show, Eq)

instance ModelName (Model TestM Anthropic) where
  modelName _ = "claude-haiku-4-5-20251001"

instance HasTools (Model TestM Anthropic) where
  withTools = anthropicTools

-- | The canonical model to use in skill tests.
--
-- Use this with @\@TestModel@ wherever a model type is needed:
--
-- @
-- history <- runSkill \@TestModel "Show me my cron jobs" cronSkill
-- @
--
-- It targets @claude-haiku-4-5-20251001@ via the Anthropic provider —
-- fast and cheap for test runs, suitable for both mocked and integration tests.
type TestModel = Model TestM Anthropic

--------------------------------------------------------------------------------
-- Root runner
--------------------------------------------------------------------------------

-- | Root runner for skill tests.
--
-- Eliminates 'TestEnvironment' and 'Fail', producing an hspec-compatible
-- @IO ()@ action.  All IO is mediated through the effect stack — no
-- arbitrary IO is permitted; interpreters may only perform IO under
-- controlled, declared conditions (cache reads\/writes, live API calls
-- gated by environment variables).
--
--  * 'Fail' → @expectationFailure@
--  * @Pending@ from 'TestEnvironment' → @pendingWith@
run :: Sem '[TestEnvironment, Fail, Logging, Embed IO] a -> IO ()
run = undefined

-- | The 'TestEnvironment' effect: lets interpreters signal test outcomes
-- back to the hspec runner without performing arbitrary IO.
-- Test code never uses this directly.
data TestEnvironment (m :: Type -> Type) a

--------------------------------------------------------------------------------
-- LLM interpreters
--------------------------------------------------------------------------------

-- | Interpret the 'LLM' effect for integration tests.
--
-- Uses cached responses in playback mode (default, no credentials needed).
-- Falls back to the live API in record\/live mode, gated by @TEST_MODE@
-- and credential environment variables.  Signals @Pending@ via
-- 'TestEnvironment' on cache miss so the test is marked pending rather
-- than failing.
interpretLLM :: Sem (LLM model ': State [Message model] ': r) a -> Sem r a
interpretLLM = undefined

-- | Interpret the 'LLM' effect for deterministic tests.
--
-- Serves the provided SSE fixtures in order, never touching the network.
-- Does not require 'TestEnvironment' — fixtures are always present.
interpretLLMMocked :: [SSEFixture] -> Sem (LLM model ': State [Message model] ': r) a -> Sem r a
interpretLLMMocked = undefined

--------------------------------------------------------------------------------
-- SSE fixture helpers
--------------------------------------------------------------------------------

-- | A raw SSE byte sequence representing one LLM response.
type SSEFixture = ByteString

-- | Minimal SSE fixture for a single tool call with the given name and
-- JSON arguments string (e.g. @"{\"key\":\"value\"}"@).
toolCallFixture :: String -> String -> SSEFixture
toolCallFixture = undefined

-- | Minimal SSE fixture for a plain text response (@"Done."@), used to
-- terminate the agent loop after a tool call.
textResponseFixture :: SSEFixture
textResponseFixture = undefined

--------------------------------------------------------------------------------
-- Assertion and predicate helpers
--------------------------------------------------------------------------------

-- | Assert a condition inside a 'Sem' action, signalling failure via 'Fail'.
assert :: Member Fail r => String -> Bool -> Sem r ()
assert _   True  = return ()
assert msg False = fail msg

-- | Predicate: the named tool was called at least once in the message history.
toolWasCalled :: Text -> [Message model] -> Bool
toolWasCalled name = any $ \msg -> case msg of
  AssistantTool (ToolCall _ n _) -> n == name
  _                              -> False

-- | Predicate: the named tool was called and its arguments satisfy the predicate.
toolCalledWith :: Text -> (Value -> Bool) -> [Message model] -> Bool
toolCalledWith name p = any $ \msg -> case msg of
  AssistantTool (ToolCall _ n args) -> n == name && p args
  _                                 -> False
