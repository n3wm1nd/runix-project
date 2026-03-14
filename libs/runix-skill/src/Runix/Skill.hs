{-# LANGUAGE ExplicitForAll #-}
-- | Skill record: the unit of packaged agent capability.
--
-- A Skill bundles a set of LLM tools under a named identity.
-- The short description lives in the cabal @synopsis@ field.
-- The long description lives in the package @README.md@.
--
-- Skills are plain values — no typeclass, no magic.  The effect row @r@
-- is the caller's row; tools run in @Sem (Fail ': r)@, consistent with
-- how 'LLMTool' is typed throughout the codebase.
module Runix.Skill
  ( Skill (..)
  , runSkill
  , subagent
  ) where

import Data.Text (Text)
import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)
import Polysemy.State (State, put, get)
import UniversalLLM (Message (..), HasTools)
import UniversalLLM.Tools (LLMTool (..))
import Runix.LLM (LLM)
import Runix.Logging (Logging)

-- | A packaged skill: identity + tools.
--
-- @r@ must contain whatever effects the tools require (e.g. @Cron@,
-- @FileSystem@).  The caller provides the interpreters; the skill
-- record is effect-agnostic.
data Skill r = Skill
  { skillId    :: Text
    -- ^ Unique identifier, e.g. @"cron"@, @"example"@.
  , skillTools :: [LLMTool (Sem (Fail ': r))]
    -- ^ The tools exposed to the LLM.
  }

-- | Run a skill with a user prompt, returning the final message history.
--
-- Seeds the message state with the prompt, runs the agent loop to completion,
-- and returns the accumulated history for inspection or assertions.
runSkill
  :: forall model r.
     ( Members '[LLM model, Fail, Logging, State [Message model]] r
     , HasTools model
     )
  => Text
  -> Skill r
  -> Sem r [Message model]
runSkill prompt skill = do
  put @[Message model] [UserText prompt]
  subagent @model [] (skillTools skill)
  get @[Message model]

-- TODO: should take histoy via State
subagent :: forall model r . HasTools model =>
  [Message model] -> [LLMTool (Sem (Fail : r))] -> Sem r Text
subagent = undefined
