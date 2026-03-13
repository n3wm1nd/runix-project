-- | Example skill — the canonical reference for writing Runix skills.
--
-- = What is a skill?
--
-- A skill is a named bundle of LLM tools.  It consists of:
--
--   * An __effect__ (@Skill.Example.Effect@) declaring what the skill can do
--     as an algebraic effect, so callers can provide different interpreters
--     (real, in-memory, mocked).
--
--   * A set of __tools__ — plain Haskell functions that implement the skill's
--     capabilities and carry enough metadata (name, description, parameter
--     names) for an LLM to call them.
--
--   * A __'Skill' value__ that bundles the tools with a system prompt and an
--     identifier.  This is what you pass to 'runSkill' or to an orchestrator.
--
-- = How to write a new skill
--
-- 1. Create @skills\/your-skill\/@, copy the cabal structure from here.
--
-- 2. Define your effect in @YourSkill.Effect@.  The effect describes what
--    the skill /needs from its environment/ — e.g. reading\/writing files,
--    calling an API.  Keep it minimal and pure in shape.
--
-- 3. Write an in-memory interpreter in @YourSkill.InMemory@ for tests.
--    Optionally write a real interpreter in @YourSkill.Interpreter@.
--
-- 4. Write your tools in @YourSkill.Skill@.  Each tool is a function:
--
--    @
--    myTool :: forall r. Member YourEffect r => Arg -> Sem (Fail ':  r) Result
--    @
--
--    The @Fail ':  r@ means the tool runs in a stack that has @Fail@ at the
--    top (for user-visible error messages) plus whatever effects @r@ contains.
--    @forall r@ is essential: tools are polymorphic in the caller's effect row.
--
-- 5. Assemble the 'Skill' value.  Each tool is wrapped in 'LLMTool' so the
--    framework can extract its description for the LLM.
--
-- 6. Write tests in @test\/YourSkill\/@.  Use 'Testing.run',
--    'Testing.interpretLLM' (integration) or 'Testing.interpretLLMMocked'
--    (deterministic), layer on your in-memory interpreter, and call
--    'runSkill' with '\@TestModel'.
--
-- = Using skills outside agents
--
-- Tools are plain functions — you can call them from any Runix program:
--
-- @
-- result <- echo (EchoInput "hello")
-- @
--
-- The 'Skill' record and system prompt only matter when you are running an
-- agent loop via 'runSkill' or 'subagent'.  Ignore them otherwise.
--
-- = This file
--
-- This skill does one thing: echo text back.  It has no effect of its own
-- (no IO, no state), so the tools run in a bare @Sem (Fail ':  r)@.  Real
-- skills will add their own effect to @r@; see @skills\/cron\/@ for an
-- example with a non-trivial effect.
module Skill.Example
  ( -- * Skill values
    --
    -- Two constructors are provided to demonstrate the two prompt patterns.
    -- In a real skill you would typically expose only one.
    exampleSkill
  , exampleSkillFromPrompts
    -- * Tool (exported so callers can use it directly)
  , echo
    -- * Types
  , EchoInput (..)
  , EchoResult (..)
  ) where

import Data.Text (Text)
import Polysemy (Sem)
import Polysemy.Fail (Fail)
import Autodocodec (HasCodec (..))
import UniversalLLM.Tools (ToolFunction (..), ToolParameter (..), LLMTool (..))
import Runix.LLM.ToolInstances ()  -- Callable/ToolFunction instances for Sem
import Runix.Skill (Skill (..))
import qualified Skill.Example.Prompt as Prompt

--------------------------------------------------------------------------------
-- Parameter and result types
--
-- Each tool argument and result needs three things:
--   * A newtype wrapper (for type safety — prevents mixing up arguments)
--   * HasCodec (for JSON serialisation/deserialisation by the LLM layer)
--   * ToolParameter (for the name and description shown to the LLM)
--
-- Derive HasCodec via the underlying type when the representation is
-- identical (e.g. a newtype over Text).
--------------------------------------------------------------------------------

newtype EchoInput = EchoInput Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter EchoInput where
  paramName        _ _ = "input"
  paramDescription _   = "the text to echo back"

newtype EchoResult = EchoResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Results also need ToolParameter (for the JSON schema) and ToolFunction
-- (for the tool name and description that appear in the LLM's tool list).
instance ToolParameter EchoResult where
  paramName        _ _ = "result"
  paramDescription _   = "the echoed text, unchanged"

instance ToolFunction EchoResult where
  toolFunctionName        _ = "echo"
  toolFunctionDescription _ = "Echo the input text back unchanged"

--------------------------------------------------------------------------------
-- Tools
--
-- A tool is a function with signature:
--
--   myTool :: forall r. Member MyEffect r => Arg -> Sem (Fail ': r) Result
--
-- The 'forall r' keeps the tool polymorphic in the caller's effect row.
-- 'Fail' is prepended so the tool can fail with a user-visible message.
-- Effects the tool needs (e.g. Cron, FileSystem) go in the constraint.
--
-- This echo tool has no effects of its own, so the constraint is empty.
--------------------------------------------------------------------------------

-- | Echo the input text back unchanged.
--
-- This is the simplest possible tool: pure, no effects, cannot fail.
-- A real tool would have effects in the constraint and might call @fail@.
echo :: forall r. EchoInput -> Sem (Fail ': r) EchoResult
echo (EchoInput t) = return (EchoResult t)

--------------------------------------------------------------------------------
-- Skill assembly
--
-- The Skill record bundles the identity, system prompt, and tools.
-- skillSystemPrompt is the standing instruction given to the agent;
-- it should describe what the skill can do and any important constraints.
--
-- Each tool is wrapped in LLMTool, which extracts the description metadata
-- (via ToolFunction / ToolParameter) for the LLM's tool list.
--
-- The 'forall r' on the skill value propagates down to each tool, keeping
-- everything polymorphic in the caller's effect row.
--------------------------------------------------------------------------------

-- | Example skill with an inline system prompt.
--
-- This is the __inline pattern__: the system prompt is a plain 'Text'
-- constant in source.  Skill construction is pure — no 'IO' required.
-- Good for short prompts or when you want the skill to be a simple value.
--
-- See 'exampleSkillFromPrompts' for the file-based alternative.
exampleSkill :: forall r. Skill r
exampleSkill = exampleSkillFromPrompts Prompt.Prompts { Prompt.agent = Prompt.defaultAgent }

-- | Example skill with prompts loaded from @prompts\/*.md@ files.
--
-- This is the __file-based pattern__: prompts live in standalone @.md@ files
-- distributed with the package and loaded at startup via 'Prompt.loadPrompts'.
-- Good for longer prompts you want to read and edit without touching Haskell.
--
-- Typical usage:
--
-- @
-- main :: IO ()
-- main = do
--   prompts <- Skill.Example.Prompt.loadPrompts
--   let skill = exampleSkillFromPrompts prompts
--   ...
-- @
exampleSkillFromPrompts :: forall r. Prompt.Prompts -> Skill r
exampleSkillFromPrompts prompts = Skill
  { skillId           = "example"
  , skillSystemPrompt = Prompt.agent prompts
  , skillTools        = [ LLMTool (echo @r) ]
  }
