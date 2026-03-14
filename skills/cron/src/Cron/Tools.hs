{-# LANGUAGE UndecidableInstances #-}
-- | Cron tool result\/parameter types and 'ToolFunction' instances.
--
-- The actual tool logic lives in "Cron.Skill"; these are the wire types.
module Cron.Tools
  ( -- * Parameter types
    CronPattern (..)
  , CronSchedule (..)
  , CronCommand (..)
  , CronCommentField (..)
    -- * Result types
  , CronListResult (..)
  , CronAddResult (..)
  , CronRemoveResult (..)
  , CronEditResult (..)
  ) where

import Data.Text (Text)
import Autodocodec (HasCodec (..))
import UniversalLLM.Tools (ToolFunction (..), ToolParameter (..))

--------------------------------------------------------------------------------
-- Parameter types
--------------------------------------------------------------------------------

newtype CronPattern = CronPattern Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronPattern where
  paramName        _ _ = "pattern"
  paramDescription _   = "substring to match against cron entries"

newtype CronSchedule = CronSchedule Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronSchedule where
  paramName        _ _ = "schedule"
  paramDescription _   = "cron schedule expression, e.g. '0 * * * *' or '@daily'"

newtype CronCommand = CronCommand Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronCommand where
  paramName        _ _ = "command"
  paramDescription _   = "the shell command to run"

-- | Optional comment field — distinct newtype so the LLM schema marks it optional.
newtype CronCommentField = CronCommentField Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronCommentField where
  paramName        _ _ = "comment"
  paramDescription _   = "optional human-readable comment (omit the leading #)"

--------------------------------------------------------------------------------
-- Result types
--------------------------------------------------------------------------------

newtype CronListResult = CronListResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronListResult where
  paramName        _ _ = "cron_list_result"
  paramDescription _   = "the current crontab with line numbers"

instance ToolFunction CronListResult where
  toolFunctionName        _ = "cron_list"
  toolFunctionDescription _ =
    "List all current cron jobs with line numbers. Call this first to see what exists."

----

newtype CronAddResult = CronAddResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronAddResult where
  paramName        _ _ = "cron_add_result"
  paramDescription _   = "confirmation of the added cron entry"

instance ToolFunction CronAddResult where
  toolFunctionName        _ = "cron_add"
  toolFunctionDescription _ =
    "Add a new cron job. Provide a schedule (e.g. '0 * * * *'), a command, and an optional comment."

----

newtype CronRemoveResult = CronRemoveResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronRemoveResult where
  paramName        _ _ = "cron_remove_result"
  paramDescription _   = "confirmation or error message for the removal"

instance ToolFunction CronRemoveResult where
  toolFunctionName        _ = "cron_remove"
  toolFunctionDescription _ =
    "Remove a cron job whose entry contains the given pattern. \
    \Fails if zero or more than one entry matches — be specific."

----

newtype CronEditResult = CronEditResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronEditResult where
  paramName        _ _ = "cron_edit_result"
  paramDescription _   = "old and new cron entry for confirmation"

instance ToolFunction CronEditResult where
  toolFunctionName        _ = "cron_edit"
  toolFunctionDescription _ =
    "Change the schedule of an existing cron job matched by pattern. \
    \Fails if zero or more than one entry matches."
