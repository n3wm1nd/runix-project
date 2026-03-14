-- | Cron skill assembly: tool functions + 'Skill' value.
--
-- Tool functions live here (not in "Cron.Tools") because they need the
-- 'Cron' effect.  "Cron.Tools" only contains the wire types.
module Cron.Skill
  ( -- * Skill value
    cronSkill
    -- * Individual tool functions (exported for unit testing)
  , cronList
  , cronAdd
  , cronRemove
  , cronEdit
  ) where

import qualified Data.Text as T
import Polysemy (Sem, Member, Members)
import Polysemy.Fail (Fail)

import UniversalLLM.Tools (LLMTool (..))
import Runix.LLM.ToolInstances ()  -- Callable/Tool instances for Sem
import Runix.Skill (Skill (..))
import Cron.Effect
  ( Cron, listCrontab, setCrontab
  , CrontabText (..)
  , formatCrontab
  )
import Cron.Tools

--------------------------------------------------------------------------------
-- Tool functions
--------------------------------------------------------------------------------

cronList :: forall r. Member Cron r => Sem (Fail ': r) CronListResult
cronList = do
  ct <- listCrontab
  let formatted = formatCrontab ct
  return $ CronListResult $
    if T.null (T.strip formatted)
      then "(crontab is empty)"
      else formatted

cronAdd
  :: forall r. Member Cron r
  => CronSchedule
  -> CronCommand
  -> Maybe CronCommentField
  -> Sem (Fail ': r) CronAddResult
cronAdd (CronSchedule sched) (CronCommand cmd) maybeComment = do
  CrontabText existing <- listCrontab
  let comment = case maybeComment of
        Just (CronCommentField c) | not (T.null (T.strip c)) -> " # " <> T.strip c
        _ -> ""
      newLine = sched <> " " <> cmd <> comment
      updated = if T.null (T.strip existing)
                then newLine <> "\n"
                else existing <> newLine <> "\n"
  setCrontab (CrontabText updated)
  return $ CronAddResult $ "Added: " <> newLine

cronRemove
  :: forall r. Member Cron r
  => CronPattern
  -> Sem (Fail ': r) CronRemoveResult
cronRemove (CronPattern pat) = do
  CrontabText raw <- listCrontab
  let allLines  = T.lines raw
      matches   = filter (T.isInfixOf pat) allLines
      remaining = filter (not . T.isInfixOf pat) allLines
  case matches of
    [] ->
      return $ CronRemoveResult $
        "No cron entry matching '" <> pat <> "' found."
    [removed] -> do
      setCrontab $ CrontabText (T.unlines remaining)
      return $ CronRemoveResult $ "Removed: " <> removed
    multiple ->
      return $ CronRemoveResult $
        "Ambiguous: " <> T.pack (show (length multiple))
        <> " entries match '" <> pat <> "'. Be more specific:\n"
        <> T.unlines multiple

cronEdit
  :: forall r. Member Cron r
  => CronPattern
  -> CronSchedule
  -> Sem (Fail ': r) CronEditResult
cronEdit (CronPattern pat) (CronSchedule newSched) = do
  CrontabText raw <- listCrontab
  let allLines = T.lines raw
      matches  = filter (T.isInfixOf pat) allLines
  case matches of
    [] ->
      return $ CronEditResult $
        "No cron entry matching '" <> pat <> "' found."
    [old] -> do
      let updated = map (replaceLine old) allLines
      setCrontab $ CrontabText (T.unlines updated)
      let newLine = replaceSchedule newSched old
      return $ CronEditResult $
        "Changed:\n  from: " <> old <> "\n  to:   " <> newLine
    multiple ->
      return $ CronEditResult $
        "Ambiguous: " <> T.pack (show (length multiple))
        <> " entries match '" <> pat <> "'. Be more specific:\n"
        <> T.unlines multiple
  where
    replaceLine _ line
      | T.isInfixOf pat line = replaceSchedule newSched line
      | otherwise             = line

    replaceSchedule sched line =
      let tokens = T.words line
          (_, rest) = splitSchedule tokens
      in sched <> " " <> T.unwords rest

    splitSchedule [] = ([], [])
    splitSchedule ts@(t:trest)
      | "@" `T.isPrefixOf` t = ([t], trest)
      | otherwise             = splitAt 5 ts

--------------------------------------------------------------------------------
-- Skill assembly
--------------------------------------------------------------------------------

cronSkill :: forall r. Members '[Cron, Fail] r => Skill r
cronSkill = Skill
  { skillId    = "cron"
  , skillTools =
      [ LLMTool (cronList @r)
      , LLMTool (cronAdd @r)
      , LLMTool (cronRemove @r)
      , LLMTool (cronEdit @r)
      ]
  }
