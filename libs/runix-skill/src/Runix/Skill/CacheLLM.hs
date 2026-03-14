-- | LLM caching interceptor.
--
-- Currently a no-op identity wrapper.  The real implementation will
-- intercept 'QueryLLM' operations and record\/replay responses keyed on
-- the serialised message history, using 'UniversalLLM.Serialization'.
--
-- Call sites reference 'cacheLLM' today; when the real implementation
-- lands the signature stays the same and no skill test code changes.
module Runix.Skill.CacheLLM
  ( CachePath (..)
  , cacheLLM
  ) where

import Polysemy (Sem)

newtype CachePath = CachePath FilePath
  deriving stock (Show, Eq)

-- | Identity interceptor — passes through unchanged.
cacheLLM :: CachePath -> Sem r a -> Sem r a
cacheLLM _ = id
