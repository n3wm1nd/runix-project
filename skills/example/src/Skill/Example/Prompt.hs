-- | Prompts for the Example skill.
--
-- = Two patterns for skill prompts
--
-- __Inline__ ('defaultAgent'): a plain 'Text' constant defined directly in
-- Haskell.  Good for short prompts — no effects required, skill construction
-- stays pure.
--
-- __File-based__ ('loadPrompts'): prompts are loaded via the 'PromptLoader'
-- effect.  The interpreter decides where they come from (installed data files,
-- a database, etc.) — the skill never sees a file path or touches IO.
-- Good for longer prompts you want to distribute and edit as standalone
-- @.md@ documents.
--
-- Both are shown in 'Skill.Example': 'exampleSkill' uses 'defaultAgent'
-- (pure), 'exampleSkillFromPrompts' takes a 'Prompts' record loaded via
-- 'loadPrompts'.
module Skill.Example.Prompt
  ( -- * File-based prompts
    Prompts (..)
  , loadPrompts
    -- * Inline fallback
  , defaultAgent
  ) where

import Data.Text (Text)
import Polysemy (Sem, Member)
import Runix.Skill.PromptLoader (PromptLoader, loadPrompt)

-- | All prompts for the Example skill.
data Prompts = Prompts
  { agent :: Text
    -- ^ Standing instructions for the echo agent (@prompts\/agent.md@).
  }

-- | Load all prompts via the 'PromptLoader' effect.
--
-- The interpreter resolves the actual source; skill code only names the file.
-- Call this during skill initialisation and pass the result to
-- 'Skill.Example.exampleSkillFromPrompts'.
loadPrompts :: Member PromptLoader r => Sem r Prompts
loadPrompts = Prompts <$> loadPrompt "agent.md"

-- | Inline fallback for the agent system prompt.
--
-- Use this when you want 'Skill.Example.exampleSkill' to be a pure value
-- with no effects.  Prefer 'loadPrompts' for prompts that deserve to live
-- as standalone documents.
defaultAgent :: Text
defaultAgent =
  "You are a helpful assistant with a single tool: echo. \
  \When asked to echo something, call the echo tool with \
  \exactly the text the user provided."
