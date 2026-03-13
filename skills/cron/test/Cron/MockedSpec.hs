{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
-- | Deterministic tests for the Cron skill.
--
-- The LLM is replaced by preloaded SSE fixtures, so tests are fully
-- reproducible without any API key or network access.
module Cron.MockedSpec (spec) where

import Test.Hspec
import Polysemy.State (evalState)
import qualified Data.Text as T
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM

import qualified Runix.Skill.Testing as Testing
import Runix.Skill.Testing (TestModel, toolCallFixture, textResponseFixture, assert, toolWasCalled, toolCalledWith)
import Runix.Skill (subagent, verificationSystemPrompt)
import UniversalLLM.Tools (LLMTool (..))
import Cron.Skill (cronList, cronAdd, cronRemove, cronEdit)
import Cron.Effect (CrontabText (..))
import Cron.InMemory (cronInMemory)

run fixtures = Testing.run . Testing.interpretLLMMocked fixtures

runWithCrons initial fixtures = run fixtures . evalState initial . cronInMemory

cronTools = [LLMTool cronList, LLMTool cronAdd, LLMTool cronRemove, LLMTool cronEdit]

spec :: Spec
spec = describe "Cron Skill" $ do

  it "calls cron_list when asked to show jobs" $
    runWithCrons (CrontabText "0 * * * * /bin/backup\n")
                 [toolCallFixture "cron_list" "{}", textResponseFixture] $ do
      history <- subagent @TestModel verificationSystemPrompt cronTools "Show me my current cron jobs"
      assert "cron_list was called" (toolWasCalled "cron_list" history)

  it "calls cron_add when asked to add a job" $
    runWithCrons (CrontabText "")
                 [toolCallFixture "cron_add" "{\"schedule\":\"0 * * * *\",\"command\":\"backup.sh\"}", textResponseFixture] $ do
      history <- subagent @TestModel verificationSystemPrompt cronTools "Add a cron job that runs backup.sh every hour"
      assert "cron_add was called" (toolWasCalled "cron_add" history)

  it "calls cron_remove with backup pattern" $
    runWithCrons (CrontabText "0 * * * * /bin/backup\n")
                 [toolCallFixture "cron_remove" "{\"pattern\":\"backup\"}", textResponseFixture] $ do
      history <- subagent @TestModel verificationSystemPrompt cronTools "Remove the backup cron job"
      assert "cron_remove was called" (toolWasCalled "cron_remove" history)
      assert "pattern contains 'backup'" $ toolCalledWith "cron_remove" (\args ->
        case args of
          Object o -> case KM.lookup "pattern" o of
            Just (String p) -> "backup" `T.isInfixOf` p
            _               -> False
          _ -> False) history
