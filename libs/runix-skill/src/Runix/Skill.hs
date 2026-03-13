{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Tool-description verification agent.
--
-- 'subagent' is a strict agentic loop used in skill tests to verify that
-- a skill's tool descriptions are clear and complete enough for a generic
-- LLM to use correctly.  It is intentionally opinionated:
--
--   * Any ambiguity in a tool description is surfaced rather than guessed at.
--   * Any tool error caused by a misunderstanding of the description is treated
--     as a total verification failure, not a recoverable condition.
--
-- This is not a general-purpose agent primitive — it lives here because its
-- purpose is tightly coupled to skill testing.  A real orchestration agent
-- would live in @runix@ or @runix-code@ with its own system prompt and
-- error-handling policy.
module Runix.Skill
  ( subagent
  , verificationSystemPrompt
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

-- | The system prompt used by 'subagent' during skill verification.
--
-- Instructs the LLM to treat ambiguous tool descriptions as errors and to
-- consider any tool failure caused by misunderstanding a verification failure.
verificationSystemPrompt :: Text
verificationSystemPrompt =
  "You are a tool description verifier. Your job is to fulfill the user's \
  \request using the available tools exactly as described. \
  \If a tool's description is ambiguous or unclear in any way, do not guess — \
  \report the ambiguity explicitly instead of proceeding. \
  \Any tool error that results from a misunderstanding of the tool's description \
  \is considered a total verification failure and must be reported immediately."

-- | Run a strict agentic loop for skill tool-description verification.
--
-- The message history is accumulated in @State [Message model]@ and returned
-- on completion, allowing test assertions on which tools were called and how.
subagent
  :: forall model r.
     ( Members '[LLM model, Fail, Logging, State [Message model]] r
     , HasTools model
     )
  => Text                              -- ^ System prompt (use 'verificationSystemPrompt' or a custom override)
  -> [LLMTool (Sem (Fail ': r))]      -- ^ Tools to expose to the agent
  -> Text                              -- ^ User prompt: the request to fulfill
  -> Sem r [Message model]
subagent = undefined
