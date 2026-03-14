-- | In-memory 'Cron' interpreter for tests.
--
-- Backed by @State CrontabText@.  Initialise the state with the crontab
-- content you want the test to start with, then run through 'cronInMemory'.
--
-- Example:
--
-- @
-- result <- evalState (CrontabText "0 * * * * backup.sh") $
--             cronInMemory $
--               myAction
-- @
module Cron.InMemory
  ( cronInMemory
  ) where

import Polysemy (Sem, Member, interpret)
import Polysemy.State (State, get, put)
import Cron.Effect (Cron (..), CrontabText)

-- | Interpret 'Cron' with an in-memory 'State'.
cronInMemory
  :: Member (State CrontabText) r
  => Sem (Cron ': r) a
  -> Sem r a
cronInMemory = interpret $ \case
  ListCrontab      -> get @CrontabText
  SetCrontab cronT -> put @CrontabText cronT
