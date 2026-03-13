{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
-- | Integration tests for the Cron skill.
--
-- Runs the full agent loop against a real (or cached) LLM.
-- The real crontab is never touched — 'cronInMemory' handles the Cron effect.
module Cron.IntegrationSpec (spec) where

import Test.Hspec
import Polysemy.State (evalState)
import qualified Data.Text as T
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM

import qualified Runix.Skill.Testing as Testing
import Runix.Skill.Testing (TestModel, assert, toolWasCalled, toolCalledWith)
import Runix.Skill (runSkill)
import Cron.Skill (cronSkill)
import Cron.Effect (CrontabText (..))
import Cron.InMemory (cronInMemory)

run = Testing.run . Testing.interpretLLM

runWithCrons initial = run . evalState initial . cronInMemory

spec :: Spec
spec = describe "Cron Skill" $ do

  it "calls cron_list when asked to show jobs" $
    runWithCrons (CrontabText "0 * * * * /bin/backup\n") $ do
      history <- runSkill @TestModel "Show me my current cron jobs" cronSkill
      assert "cron_list was called" (toolWasCalled "cron_list" history)

  it "calls cron_add when asked to add a job" $
    runWithCrons (CrontabText "") $ do
      history <- runSkill @TestModel "Add a cron job that runs backup.sh every hour" cronSkill
      assert "cron_add was called" (toolWasCalled "cron_add" history)

  it "calls cron_remove with backup pattern" $
    runWithCrons (CrontabText "0 * * * * /bin/backup\n") $ do
      history <- runSkill @TestModel "Remove the backup cron job" cronSkill
      assert "cron_remove was called" (toolWasCalled "cron_remove" history)
      assert "pattern contains 'backup'" $ toolCalledWith "cron_remove" (\args ->
        case args of
          Object o -> case KM.lookup "pattern" o of
            Just (String p) -> "backup" `T.isInfixOf` p
            _               -> False
          _ -> False) history
