{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Skill record and the @subagent@ primitive.
--
-- A 'Skill' bundles a set of LLM tools under a named identity.
-- 'subagent' is the single entry point for running an agentic loop:
-- it owns the system prompt, the tools, and the user prompt, while
-- message history lives in @State [Message model]@ so callers can
-- inspect or assert on it after the call.
--
-- The short skill description lives in the cabal @synopsis@ field.
-- The long description lives in the package @README.md@.
--
-- Skills are plain values — no typeclass, no magic.  The effect row @r@
-- is the caller's row; tools run in @Sem (Fail ': r)@, consistent with
-- how 'LLMTool' is typed throughout the codebase.
module Runix.Skill
  ( -- * The subagent primitive
    subagent
    -- * Skill record
  , Skill (..)
    -- * Convenience entry point
  , runSkill
  ) where

import Data.Text (Text)
import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import UniversalLLM (Message (..), HasTools)
import UniversalLLM.Tools (LLMTool (..))
import Runix.LLM (LLM)
import Runix.Logging (Logging)

-- | Run an agentic loop with a given system prompt, tools, and user prompt.
--
-- The message history is read from and written to @State [Message model]@.
-- After @subagent@ returns the state holds the full conversation, which is
-- useful for assertions in tests.
--
-- This is the primitive all skills and skill tools are built on.
-- @runSkill@ is just a thin wrapper that seeds the state and calls @subagent@.
subagent
  :: forall model r.
     ( Members '[LLM model, Fail, Logging, State [Message model]] r
     , HasTools model
     )
  => Text                              -- ^ System prompt: the agent's standing instructions
  -> [LLMTool (Sem (Fail ': r))]      -- ^ Tools available to the agent
  -> Text                              -- ^ User prompt: the specific request for this call
  -> Sem r [Message model]
subagent = undefined

-- | A packaged skill: identity + tools.
--
-- @r@ must contain whatever effects the tools require (e.g. @Cron@,
-- @FileSystem@).  The caller provides the interpreters; the skill
-- record is effect-agnostic.
--
-- Skills are not just for agent use — tools are plain functions in @Sem r@
-- and can be called directly from any Runix program.  The LLM descriptions
-- and 'Skill' wrapper are there when you want agent-driven dispatch; ignore
-- them when you just want to call @cronAdd@ directly.
data Skill r = Skill
  { skillId          :: Text
    -- ^ Unique identifier, e.g. @"cron"@, @"example"@.
  , skillSystemPrompt :: Text
    -- ^ Standing instructions for the agent running this skill.
  , skillTools       :: [LLMTool (Sem (Fail ': r))]
    -- ^ The tools exposed to the LLM (and available for direct calls).
  }

-- | Run a skill with a user prompt, returning the final message history.
--
-- Seeds the message state with @[UserText prompt]@, delegates to 'subagent',
-- and returns the accumulated history.  Useful as a clean entry point in
-- tests and in agent orchestration.
runSkill
  :: forall model r.
     ( Members '[LLM model, Fail, Logging, State [Message model]] r
     , HasTools model
     )
  => Text
  -> Skill r
  -> Sem r [Message model]
runSkill prompt skill =
  subagent @model (skillSystemPrompt skill) (skillTools skill) prompt
