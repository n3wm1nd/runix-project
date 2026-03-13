-- | Cron skill: tool functions and LLM metadata.
module Cron.Skill
  ( -- * Tool functions (exported for direct use and unit testing)
    cronList
  , cronAdd
  , cronRemove
  , cronEdit
    -- * Types
  , CronPattern (..)
  , CronSchedule (..)
  , CronCommand (..)
  , CronCommentField (..)
  , CronListResult (..)
  , CronAddResult (..)
  , CronRemoveResult (..)
  , CronEditResult (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Sem, Member)
import Autodocodec (HasCodec (..))
import UniversalLLM.Tools (ToolFunction (..), ToolParameter (..))
import Cron.Effect
  ( Cron, listCrontab, setCrontab
  , CrontabText (..)
  , formatCrontab
  )

--------------------------------------------------------------------------------
-- cron_list
--------------------------------------------------------------------------------

newtype CronListResult = CronListResult Text deriving stock (Show, Eq) deriving (HasCodec) via Text

cronList :: Member Cron r => Sem r CronListResult
cronList = do
  ct <- listCrontab
  let formatted = formatCrontab ct
  return $ CronListResult $
    if T.null (T.strip formatted) then "(crontab is empty)" else formatted

instance ToolParameter CronListResult where
  paramName        = "cron_list_result"
  paramDescription = "the current crontab"

instance ToolFunction CronListResult where
  toolFunctionName        = "cron_list"
  toolFunctionDescription = "List all current cron jobs. Call this first to see what exists."

--------------------------------------------------------------------------------
-- cron_add
--------------------------------------------------------------------------------

newtype CronSchedule     = CronSchedule     Text deriving stock (Show, Eq) deriving (HasCodec) via Text
newtype CronCommand      = CronCommand      Text deriving stock (Show, Eq) deriving (HasCodec) via Text
newtype CronCommentField = CronCommentField Text deriving stock (Show, Eq) deriving (HasCodec) via Text
newtype CronAddResult    = CronAddResult    Text deriving stock (Show, Eq) deriving (HasCodec) via Text

cronAdd
  :: Member Cron r
  => CronSchedule
  -> CronCommand
  -> Maybe CronCommentField
  -> Sem r CronAddResult
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

instance ToolParameter CronSchedule where
  paramName        = "schedule"
  paramDescription = "cron schedule expression, e.g. '0 * * * *' or '@daily'"

instance ToolParameter CronCommand where
  paramName        = "command"
  paramDescription = "the shell command to run"

instance ToolParameter CronCommentField where
  paramName        = "comment"
  paramDescription = "optional human-readable comment (omit the leading #)"

instance ToolParameter CronAddResult where
  paramName        = "cron_add_result"
  paramDescription = "confirmation of the added cron entry"

instance ToolFunction CronAddResult where
  toolFunctionName        = "cron_add"
  toolFunctionDescription = "Add a new cron job. Provide a schedule, a command, and an optional comment."

--------------------------------------------------------------------------------
-- cron_remove
--------------------------------------------------------------------------------

newtype CronPattern      = CronPattern      Text deriving stock (Show, Eq) deriving (HasCodec) via Text
newtype CronRemoveResult = CronRemoveResult Text deriving stock (Show, Eq) deriving (HasCodec) via Text

cronRemove :: Member Cron r => CronPattern -> Sem r CronRemoveResult
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

instance ToolParameter CronPattern where
  paramName        = "pattern"
  paramDescription = "substring to match against cron entries"

instance ToolParameter CronRemoveResult where
  paramName        = "cron_remove_result"
  paramDescription = "confirmation or error message for the removal"

instance ToolFunction CronRemoveResult where
  toolFunctionName        = "cron_remove"
  toolFunctionDescription =
    "Remove a cron job whose entry contains the given pattern. \
    \Fails if zero or more than one entry matches — be specific."

--------------------------------------------------------------------------------
-- cron_edit
--------------------------------------------------------------------------------

newtype CronEditResult = CronEditResult Text deriving stock (Show, Eq) deriving (HasCodec) via Text

-- | Change the schedule of an existing cron job matched by pattern.
-- CronPattern and CronSchedule instances are already defined above.
cronEdit :: Member Cron r => CronPattern -> CronSchedule -> Sem r CronEditResult
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

instance ToolParameter CronEditResult where
  paramName        = "cron_edit_result"
  paramDescription = "old and new cron entry for confirmation"

instance ToolFunction CronEditResult where
  toolFunctionName        = "cron_edit"
  toolFunctionDescription =
    "Change the schedule of an existing cron job matched by pattern. \
    \Fails if zero or more than one entry matches."

