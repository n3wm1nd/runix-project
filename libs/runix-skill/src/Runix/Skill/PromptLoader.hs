-- | The 'PromptLoader' effect: read prompt files and skill descriptions
-- without touching IO directly.
--
-- Skill source code declares what it needs ('loadPrompt', 'loadDescription')
-- as effects.  The interpreter — provided by the runtime infrastructure, not
-- the skill — handles the actual lookup (installed data files, a database,
-- a remote store, etc.).
--
-- This keeps skill libraries in Safe Haskell: no IO, no 'Paths_' imports,
-- no knowledge of how or where prompts are stored.
module Runix.Skill.PromptLoader
  ( -- * Effect
    PromptLoader (..)
    -- * Actions
  , loadPrompt
  , loadDescription
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Polysemy (Sem, Member, send)

-- | Effect for loading skill-associated text resources.
--
-- The two constructors cover the two kinds of resource a skill typically needs:
--
--   * 'LoadPrompt' — a named prompt file from the skill's @prompts\/@ directory.
--     The interpreter resolves the filename relative to the skill's data root;
--     the skill just supplies the filename (e.g. @\"agent.md\"@).
--
--   * 'LoadDescription' — the skill's own description text.  Conceptually the
--     skill's README, but the interpreter decides the source: an installed
--     @README.md@, a database entry, a remote fetch, etc.
data PromptLoader (m :: Type -> Type) a where
  LoadPrompt      :: FilePath -> PromptLoader m Text
  LoadDescription :: PromptLoader m Text

-- | Load a prompt file by filename (e.g. @\"agent.md\"@).
--
-- The interpreter resolves the actual path; skill code never sees it.
loadPrompt :: Member PromptLoader r => FilePath -> Sem r Text
loadPrompt = send . LoadPrompt

-- | Load the skill's description text.
loadDescription :: Member PromptLoader r => Sem r Text
loadDescription = send LoadDescription
