{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
-- | Unit tests for pure cron helpers and the in-memory interpreter.
-- No LLM involved — these are fast, deterministic, and always runnable.
module Cron.InterpreterSpec (spec) where

import Test.Hspec
import Polysemy (Sem, run)
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (State, runState)
import qualified Data.Text as T

import Cron.Effect (Cron, CrontabText (..), parseCrontab, CronEntry (..))
import Cron.InMemory (cronInMemory)
import Cron.Skill (cronAdd, cronRemove, cronEdit)
import Cron.Tools (CronSchedule (..), CronCommand (..), CronPattern (..), CronCommentField (..),
                   CronAddResult (..), CronRemoveResult (..), CronEditResult (..))

type CronAction a = Sem '[Fail, Cron, State CrontabText] a

-- | Run a skill tool action with the in-memory Cron interpreter.
-- Returns (Either String result, final crontab).
runCron :: CrontabText -> CronAction a -> IO (Either String a, CrontabText)
runCron initial action = return $ swap $
  run $ runState @CrontabText initial $ cronInMemory $ runFail action
  where swap (ct, r) = (r, ct)

spec :: Spec
spec = do
  describe "parseCrontab" $ do
    it "parses standard 5-field entries" $ do
      let ct = CrontabText "0 * * * * /usr/bin/backup.sh\n"
          [entry] = parseCrontab ct
      cronSchedule entry `shouldBe` "0 * * * *"
      cronCommand  entry `shouldBe` "/usr/bin/backup.sh"
      cronComment  entry `shouldBe` Nothing

    it "parses @keyword entries" $ do
      let ct = CrontabText "@daily /usr/bin/cleanup.sh\n"
          [entry] = parseCrontab ct
      cronSchedule entry `shouldBe` "@daily"
      cronCommand  entry `shouldBe` "/usr/bin/cleanup.sh"

    it "skips comment lines" $ do
      let ct = CrontabText "# this is a comment\n0 * * * * cmd\n"
      length (parseCrontab ct) `shouldBe` 1

    it "skips blank lines" $ do
      let ct = CrontabText "\n0 * * * * cmd\n\n"
      length (parseCrontab ct) `shouldBe` 1

    it "extracts inline comments" $ do
      let ct = CrontabText "0 * * * * /bin/cmd # my job\n"
          [entry] = parseCrontab ct
      cronComment entry `shouldBe` Just "my job"

  describe "cronAdd (in-memory)" $ do
    it "appends a new entry to an empty crontab" $ do
      let action = cronAdd (CronSchedule "0 * * * *") (CronCommand "/bin/backup") Nothing
      (result, CrontabText final) <- runCron (CrontabText "") (action :: CronAction CronAddResult)
      result `shouldBe` Right (CronAddResult "Added: 0 * * * * /bin/backup")
      final `shouldSatisfy` T.isInfixOf "0 * * * * /bin/backup"

    it "appends without clobbering existing entries" $ do
      let initial = CrontabText "* * * * * existing\n"
          action = cronAdd (CronSchedule "@daily") (CronCommand "new") Nothing
      (_, CrontabText final) <- runCron initial (action :: CronAction CronAddResult)
      final `shouldSatisfy` T.isInfixOf "existing"
      final `shouldSatisfy` T.isInfixOf "@daily new"

    it "includes the comment when provided" $ do
      let action = cronAdd (CronSchedule "0 0 * * *") (CronCommand "/bin/cmd")
                           (Just (CronCommentField "nightly"))
      (result, _) <- runCron (CrontabText "") (action :: CronAction CronAddResult)
      result `shouldBe` Right (CronAddResult "Added: 0 0 * * * /bin/cmd # nightly")

  describe "cronRemove (in-memory)" $ do
    let initial = CrontabText "0 * * * * /bin/backup\n* * * * * /bin/other\n"

    it "removes a matching entry" $ do
      (result, CrontabText final) <- runCron initial
        (cronRemove (CronPattern "backup") :: CronAction CronRemoveResult)
      result `shouldBe` Right (CronRemoveResult "Removed: 0 * * * * /bin/backup")
      final `shouldSatisfy` (not . T.isInfixOf "backup")
      final `shouldSatisfy` T.isInfixOf "other"

    it "returns error on no match" $ do
      (result, _) <- runCron initial
        (cronRemove (CronPattern "nonexistent") :: CronAction CronRemoveResult)
      result `shouldBe` Right (CronRemoveResult "No cron entry matching 'nonexistent' found.")

    it "returns error on ambiguous match" $ do
      (result, _) <- runCron initial
        (cronRemove (CronPattern "/bin/") :: CronAction CronRemoveResult)
      case result of
        Right (CronRemoveResult msg) -> msg `shouldSatisfy` T.isInfixOf "Ambiguous"
        _ -> expectationFailure "Expected Right with ambiguous message"

  describe "cronEdit (in-memory)" $ do
    let initial = CrontabText "0 * * * * /bin/backup\n"

    it "replaces the schedule" $ do
      (result, CrontabText final) <- runCron initial
        (cronEdit (CronPattern "backup") (CronSchedule "@daily") :: CronAction CronEditResult)
      case result of
        Right (CronEditResult msg) -> msg `shouldSatisfy` T.isInfixOf "@daily"
        _ -> expectationFailure "Expected Right"
      final `shouldSatisfy` T.isInfixOf "@daily /bin/backup"

    it "preserves other entries" $ do
      let two = CrontabText "0 * * * * /bin/backup\n* * * * * /bin/other\n"
      (_, CrontabText final) <- runCron two
        (cronEdit (CronPattern "backup") (CronSchedule "@weekly") :: CronAction CronEditResult)
      final `shouldSatisfy` T.isInfixOf "other"
