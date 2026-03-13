{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | The @subagent@ primitive.
--
-- 'subagent' is the single entry point for running an agentic loop:
-- it owns the system prompt, the tools, and the user prompt, while
-- message history lives in @State [Message model]@ so callers can
-- inspect or assert on it after the call.
--
-- A skill is a Haskell library: its identity comes from the cabal package
-- name and synopsis, its description from the README, and its tools from
-- the functions it exports.  There is no skill record — callers assemble
-- system prompt and tools and pass them directly to 'subagent'.
module Runix.Skill
  ( subagent
  ) where

import Data.Text (Text)
import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import UniversalLLM (Message (..), HasTools)
import UniversalLLM.Tools (LLMTool (..))
import Runix.LLM (LLM)
import Runix.LLM.ToolInstances ()  -- orphan instances: Callable/Tool for Sem
import Runix.Logging (Logging)

-- | Run an agentic loop with a given system prompt, tools, and user prompt.
--
-- The message history is read from and written to @State [Message model]@.
-- After @subagent@ returns the state holds the full conversation, which is
-- useful for assertions in tests.
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
