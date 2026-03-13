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
  paramName        = "pattern"
  paramDescription = "substring to match against cron entries"

newtype CronSchedule = CronSchedule Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronSchedule where
  paramName        = "schedule"
  paramDescription = "cron schedule expression, e.g. '0 * * * *' or '@daily'"

newtype CronCommand = CronCommand Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronCommand where
  paramName        = "command"
  paramDescription = "the shell command to run"

-- | Optional comment field — distinct newtype so the LLM schema marks it optional.
newtype CronCommentField = CronCommentField Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronCommentField where
  paramName        = "comment"
  paramDescription = "optional human-readable comment (omit the leading #)"

--------------------------------------------------------------------------------
-- Result types
--------------------------------------------------------------------------------

newtype CronListResult = CronListResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronListResult where
  paramName        = "cron_list_result"
  paramDescription = "the current crontab with line numbers"

instance ToolFunction CronListResult where
  toolFunctionName        = "cron_list"
  toolFunctionDescription =
    "List all current cron jobs with line numbers. Call this first to see what exists."

----

newtype CronAddResult = CronAddResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronAddResult where
  paramName        = "cron_add_result"
  paramDescription = "confirmation of the added cron entry"

instance ToolFunction CronAddResult where
  toolFunctionName        = "cron_add"
  toolFunctionDescription =
    "Add a new cron job. Provide a schedule (e.g. '0 * * * *'), a command, and an optional comment."

----

newtype CronRemoveResult = CronRemoveResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronRemoveResult where
  paramName        = "cron_remove_result"
  paramDescription = "confirmation or error message for the removal"

instance ToolFunction CronRemoveResult where
  toolFunctionName        = "cron_remove"
  toolFunctionDescription =
    "Remove a cron job whose entry contains the given pattern. \
    \Fails if zero or more than one entry matches — be specific."

----

newtype CronEditResult = CronEditResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter CronEditResult where
  paramName       = "cron_edit_result"
  paramDescription = "old and new cron entry for confirmation"

instance ToolFunction CronEditResult where
  toolFunctionName        = "cron_edit"
  toolFunctionDescription =
    "Change the schedule of an existing cron job matched by pattern. \
    \Fails if zero or more than one entry matches."
