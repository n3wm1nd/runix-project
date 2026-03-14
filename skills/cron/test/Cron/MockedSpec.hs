{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
-- | Deterministic tests for the Cron skill.
--
-- The LLM is replaced by preloaded SSE fixtures, so tests are fully
-- reproducible without any API key or network access.
module Cron.MockedSpec (spec) where

import Test.Hspec
import Polysemy.State (evalState)
import UniversalLLM (Model, ModelName (..), HasTools (..))
import UniversalLLM.Providers.Anthropic (Anthropic (..))
import qualified UniversalLLM.Providers.Anthropic as Provider
import qualified Data.Text as T
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM

import qualified Runix.Skill.Testing as Testing
import Runix.Skill.Testing (toolCallFixture, textResponseFixture, assert, toolWasCalled, toolCalledWith)
import Runix.Skill (runSkill)
import Cron.Skill (cronSkill)
import Cron.Effect (CrontabText (..))
import Cron.InMemory (cronInMemory)

data TestM = TestM deriving stock (Show, Eq)
instance ModelName (Model TestM Anthropic) where
  modelName _ = "claude-haiku-4-5-20251001"
instance HasTools (Model TestM Anthropic) where
  withTools = Provider.anthropicTools
type CronModel = Model TestM Anthropic

run fixtures = Testing.run . Testing.interpretLLMMocked fixtures

runWithCrons initial fixtures = run fixtures . evalState initial . cronInMemory

spec :: Spec
spec = describe "Cron Skill" $ do

  it "calls cron_list when asked to show jobs" $
    runWithCrons (CrontabText "0 * * * * /bin/backup\n")
                 [toolCallFixture "cron_list" "{}", textResponseFixture] $ do
      history <- runSkill @CronModel "Show me my current cron jobs" cronSkill
      assert "cron_list was called" (toolWasCalled "cron_list" history)

  it "calls cron_add when asked to add a job" $
    runWithCrons (CrontabText "")
                 [toolCallFixture "cron_add" "{\"schedule\":\"0 * * * *\",\"command\":\"backup.sh\"}", textResponseFixture] $ do
      history <- runSkill @CronModel "Add a cron job that runs backup.sh every hour" cronSkill
      assert "cron_add was called" (toolWasCalled "cron_add" history)

  it "calls cron_remove with backup pattern" $
    runWithCrons (CrontabText "0 * * * * /bin/backup\n")
                 [toolCallFixture "cron_remove" "{\"pattern\":\"backup\"}", textResponseFixture] $ do
      history <- runSkill @CronModel "Remove the backup cron job" cronSkill
      assert "cron_remove was called" (toolWasCalled "cron_remove" history)
      assert "pattern contains 'backup'" $ toolCalledWith "cron_remove" (\args ->
        case args of
          Object o -> case KM.lookup "pattern" o of
            Just (String p) -> "backup" `T.isInfixOf` p
            _               -> False
          _ -> False) history
