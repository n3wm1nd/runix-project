-- | Example skill — the canonical reference for writing Runix skills.
--
-- = What is a skill?
--
-- A skill is a collection of LLM-callable tools, packaged as a Haskell
-- library.  It consists of:
--
--   * An optional __effect__ declaring what the skill needs from its
--     environment (e.g. reading a crontab, calling an API).  Effects are not
--     required — a skill with purely computational tools needs none — but they
--     are the right abstraction when you want to separate /what/ should happen
--     from /how/ it is done.  An effect lets callers swap in different
--     interpreters: a real one, an in-memory one for tests, a mock.
--
--   * A set of __tools__ — plain Haskell functions that implement the skill's
--     capabilities.  Each function carries enough metadata (name, description,
--     parameter names) for an LLM to call it correctly.
--
--   * __Prompts__ in @prompts\/*.md@, loaded via the 'PromptLoader' effect so
--     the skill source stays pure.  Short prompts can also be inlined as
--     'Text' constants — see 'Skill.Example.Prompt.defaultAgent'.
--
-- = How to write a new skill
--
-- 1. Create @skills\/your-skill\/@, copy the cabal structure from here.
--
-- 2. If your tools need to interact with the environment, define an effect in
--    @YourSkill.Effect@.  Keep it minimal — one constructor per distinct
--    operation.  If your tools are purely computational, skip this step.
--
-- 3. Write an in-memory interpreter in @YourSkill.InMemory@ for tests, and
--    a real interpreter in @YourSkill.Interpreter@ for production use.
--
-- 4. Write your tools in @YourSkill.Skill@ (this file).  Each tool is a
--    self-contained block: data types, then the function, then LLM metadata
--    instances.  Skip instances that are already covered by a previous tool.
--
-- 5. Write prompts in @prompts\/*.md@ and load them via 'PromptLoader' in
--    @YourSkill.Prompt@.  See 'Skill.Example.Prompt' for both the inline and
--    file-based patterns.
--
-- 6. Write tests in @test\/YourSkill\/@.  Use 'Testing.run',
--    'Testing.interpretLLM' (integration) or 'Testing.interpretLLMMocked'
--    (deterministic), layer on your in-memory interpreter, and call
--    'runSkill' with '\@TestModel'.
--
-- = Using skills outside agents
--
-- Tools are plain functions — you can call them from any Runix program
-- without involving an agent or an LLM at all:
--
-- @
-- result <- echo (EchoInput \"hello\")
-- @
--
-- The LLM metadata (tool name, descriptions) and 'Skill' wrapper only matter
-- when you want agent-driven dispatch.  Otherwise just import the functions
-- you need and use them directly.
--
-- = This skill
--
-- A single @echo@ tool: returns its input unchanged.  No effect required —
-- the tool is purely computational.  This makes it the simplest possible
-- example of the skill structure.  See @skills\/cron\/@ for a skill with a
-- non-trivial effect, an in-memory interpreter, and a richer tool set.
module Skill.Example
  ( -- * Tools
    echo
  , echoLoud
  , echoSimple
    -- * Types
  , EchoInput (..)
  , EchoResult (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Sem, Member)
import Polysemy.Fail (Fail)
import Autodocodec (HasCodec (..))
import UniversalLLM.Tools (ToolFunction (..), ToolParameter (..), mkTool, ToolWrapped)
import Runix.Skill ()  -- pulls in orphan Callable/Tool instances for Sem

--------------------------------------------------------------------------------
-- echo
--
-- The simplest possible tool: purely computational, no constraints, cannot
-- fail.  The bare @Sem r@ with no constraint means this function makes no
-- demands on the effect row at all — it works in any context.
--------------------------------------------------------------------------------

newtype EchoInput  = EchoInput  Text deriving stock (Show, Eq) deriving (HasCodec) via Text
newtype EchoResult = EchoResult Text deriving stock (Show, Eq) deriving (HasCodec) via Text

echo :: EchoInput -> Sem r EchoResult
echo (EchoInput t) = return (EchoResult t)

instance ToolParameter EchoInput where
  paramName        _ _ = "input"
  paramDescription _   = "the text to echo back"

instance ToolParameter EchoResult where
  paramName        _ _ = "result"
  paramDescription _   = "the echoed text, unchanged"

instance ToolFunction EchoResult where
  toolFunctionName        _ = "echo"
  toolFunctionDescription _ = "Echo the input text back unchanged"

--------------------------------------------------------------------------------
-- echoLoud
--
-- Demonstrates precondition checking via 'fail'.  @Member Fail r@ expresses
-- that this tool calls 'fail' under some conditions; compare with 'echo' which
-- has no constraints at all.
--
-- 'Fail' is always available during tool execution regardless of what @r@
-- contains — tools don't need to know where it comes from or who catches it.
-- Any failure is reported back to the LLM as a tool error, never propagated
-- into the surrounding program.
--
-- 'EchoInput' is reused from 'echo'; no need to repeat its instances.
--------------------------------------------------------------------------------

newtype EchoLoudResult = EchoLoudResult Text deriving stock (Show, Eq) deriving (HasCodec) via Text

echoLoud :: Member Fail r => EchoInput -> Sem r EchoLoudResult
echoLoud (EchoInput t)
  | T.null (T.strip t) = fail "input must not be empty"
  | otherwise          = return (EchoLoudResult (T.toUpper t))

instance ToolParameter EchoLoudResult where
  paramName        _ _ = "result"
  paramDescription _   = "the echoed text in uppercase"

instance ToolFunction EchoLoudResult where
  toolFunctionName        _ = "echo_loud"
  toolFunctionDescription _ = "Echo the input text back in uppercase. Fails if the input is empty."

--------------------------------------------------------------------------------
-- echoSimple
--
-- The same as 'echo' but defined with 'mkTool' instead of a 'ToolFunction'
-- instance on the result type.
--
-- 'mkTool' describes the /function/: name and description are tied to this
-- specific wrapped instance.  'ToolFunction' describes the /result type/:
-- any function that produces an 'EchoResult' is automatically a valid tool,
-- regardless of how many arguments it takes or how it gets there, and each
-- parameter describes itself independently via 'ToolParameter'.
--
-- Use 'mkTool' when you have no unique return type to hang a 'ToolFunction'
-- instance on.  If you do have (or can newtype) a distinct result type,
-- 'ToolFunction' is strictly better: multiple implementations can coexist,
-- transformations compose naturally before wrapping, and the description
-- travels with the type rather than with one particular function.
--
-- No new instances needed — 'EchoInput' and 'EchoResult' are already covered.
--------------------------------------------------------------------------------

echoSimple :: ToolWrapped (EchoInput -> Sem r EchoResult) (EchoInput, ())
echoSimple = mkTool "echo_simple" "Echo the input text back unchanged" echo
