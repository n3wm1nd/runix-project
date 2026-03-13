-- | Prompts for the Cron skill.
--
-- See 'Skill.Example.Prompt' for a full description of the two patterns
-- (inline and file-based).  The cron skill uses both:
--
--   * 'defaultAgent' — an inline fallback for when no prompt files are present.
--   * 'loadPrompts'  — loads @prompts\/agent.md@ via the 'PromptLoader' effect.
module Cron.Prompt
  ( -- * File-based prompts
    Prompts (..)
  , loadPrompts
    -- * Inline fallback
  , defaultAgent
  ) where

import Data.Text (Text)
import Polysemy (Sem, Member)
import Runix.Skill.PromptLoader (PromptLoader, loadPrompt)

-- | All prompts for the Cron skill.
data Prompts = Prompts
  { agent :: Text
    -- ^ Standing instructions for the cron agent (@prompts\/agent.md@).
  }

-- | Load all prompts via the 'PromptLoader' effect.
loadPrompts :: Member PromptLoader r => Sem r Prompts
loadPrompts = Prompts <$> loadPrompt "agent.md"

-- | Inline fallback for the agent system prompt.
defaultAgent :: Text
defaultAgent =
  "You are a cron job manager. Help the user manage their crontab: \
  \list jobs, add new scheduled tasks, remove existing ones, and edit \
  \schedules. Always confirm what you did after making changes."
