-- | Real 'Cron' interpreter: shells out to @crontab(1)@.
--
-- Requires 'Cmd "crontab"' and 'Fail' in the effect row.
-- @crontab -l@ reads the current crontab; @crontab -@ writes it from stdin.
module Cron.Interpreter
  ( cronIO
  ) where

import Data.Text.Encoding (encodeUtf8)
import Polysemy (Sem, Members, interpret)
import Polysemy.Fail (Fail)

import Runix.Cmd (Cmd, call, callStdin, exitCode, stdout)
import Cron.Effect (Cron (..), CrontabText (..))

-- | Interpret 'Cron' by invoking the real @crontab@ command.
cronIO
  :: Members '[Cmd "crontab", Fail] r
  => Sem (Cron ': r) a
  -> Sem r a
cronIO = interpret $ \case
  ListCrontab -> do
    out <- call @"crontab" ["-l"]
    -- Exit code 1 with "no crontab" message is normal on some systems
    if exitCode out == 0
      then return $ CrontabText (stdout out)
      else if "no crontab for" `elem` words (show (stdout out))
        then return $ CrontabText ""
        else fail $ "crontab -l failed: " <> show (stdout out)

  SetCrontab (CrontabText content) -> do
    out <- callStdin @"crontab" ["-"] (encodeUtf8 content)
    if exitCode out == 0
      then return ()
      else fail $ "crontab - failed: " <> show (stdout out)
