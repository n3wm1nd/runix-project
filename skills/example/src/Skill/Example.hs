{-# LANGUAGE UndecidableInstances #-}
-- | Example skill — echo tool.
--
-- Demonstrates the minimal pattern for a Runix skill:
--   1. Newtype parameter and result types with 'HasCodec' and 'ToolParameter'
--   2. A result type with 'ToolFunction'
--   3. A tool function in @Sem r@
--   4. A 'Skill' value that assembles everything
module Skill.Example
  ( -- * Skill value
    exampleSkill
    -- * Tool (exported for testing)
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
import Runix.LLM.ToolInstances ()  -- Callable/Tool instances for Sem
import Runix.Skill (Skill (..))

--------------------------------------------------------------------------------
-- Parameter types
--------------------------------------------------------------------------------

newtype EchoInput = EchoInput Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter EchoInput where
  paramName        _ _ = "input"
  paramDescription _   = "the text to echo back"

--------------------------------------------------------------------------------
-- Result type
--------------------------------------------------------------------------------

newtype EchoResult = EchoResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter EchoResult where
  paramName        _ _ = "echo_result"
  paramDescription _   = "the echoed text"

instance ToolFunction EchoResult where
  toolFunctionName        _ = "echo"
  toolFunctionDescription _ = "Echo the input text back unchanged"

--------------------------------------------------------------------------------
-- Skill assembly
--------------------------------------------------------------------------------

exampleSkill :: forall r. Skill r
exampleSkill = Skill
  { skillId    = "example"
  , skillTools = [ LLMTool (echo @r) ]
  }

echo :: forall r. EchoInput -> Sem (Fail ': r) EchoResult
echo (EchoInput t) = return (EchoResult t)
