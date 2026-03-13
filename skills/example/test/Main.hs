{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
-- | Test suite for the Example skill.
--
-- Demonstrates both test tiers:
--
--   * Mocked tests — deterministic, no network, uses SSE fixtures.
--   * Integration tests — uses cached\/live LLM via 'Testing.interpretLLM'.
--
-- See @Runix.Skill.Testing@ for the full infrastructure.
module Main (main) where

import Test.Hspec
import qualified Runix.Skill.Testing as Testing
import Runix.Skill.Testing (TestModel, toolCallFixture, textResponseFixture, assert, toolWasCalled)
import Runix.Skill (subagent, verificationSystemPrompt)
import UniversalLLM.Tools (LLMTool (..))
import Skill.Example (echo, echoLoud)

--------------------------------------------------------------------------------
-- Test runners
--
-- 'runMocked' and 'runIntegration' are the entry points for the two test tiers.
-- Skills that need additional effects (e.g. a Cron or FileSystem interpreter)
-- would layer them on top in a 'runWith...' combinator — see the cron skill
-- tests for an example.
--
-- For this skill there are no extra effects, so the runners are one-liners.
--------------------------------------------------------------------------------

runMocked fixtures = Testing.run . Testing.interpretLLMMocked fixtures

runIntegration = Testing.run . Testing.interpretLLM

echoTools = [LLMTool echo, LLMTool echoLoud]

--------------------------------------------------------------------------------
-- Mocked tests
--
-- Preloaded SSE fixtures drive the LLM, so tests are fully reproducible
-- without any API key or network access.  Use these for:
--   * Verifying that a specific tool is called for a given prompt.
--   * Verifying that the tool is called with the right arguments.
--
-- Fixture pattern: one toolCallFixture per tool call the model should make,
-- followed by a textResponseFixture to terminate the loop.
--------------------------------------------------------------------------------

mockedSpec :: Spec
mockedSpec = describe "mocked" $ do

  it "calls echo when asked to echo" $
    runMocked [toolCallFixture "echo" "{\"input\":\"hello\"}", textResponseFixture] $ do
      history <- subagent @TestModel verificationSystemPrompt echoTools "Please echo 'hello'"
      assert "echo was called" (toolWasCalled "echo" history)

--------------------------------------------------------------------------------
-- Integration tests
--
-- Uses the real LLM (cached in playback mode, so no API key needed by default).
-- If a cached response exists the test runs deterministically; if not, the
-- test is marked pending until a response is recorded.
--------------------------------------------------------------------------------

integrationSpec :: Spec
integrationSpec = describe "integration" $ do

  it "calls echo when asked to echo" $
    runIntegration $ do
      history <- subagent @TestModel verificationSystemPrompt echoTools "Please echo the word 'hello'"
      assert "echo was called" (toolWasCalled "echo" history)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ describe "Example Skill" $ do
  mockedSpec
  integrationSpec
