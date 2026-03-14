{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
-- | The Cron effect: abstract operations on the user's crontab.
--
-- Two interpreters are provided:
--
--   * 'Cron.Interpreter.cronIO'       — real interpreter via @crontab@ command
--   * 'Cron.InMemory.cronInMemory'    — in-memory interpreter for tests
module Cron.Effect
  ( -- * Effect
    Cron (..)
    -- * Smart constructors
  , listCrontab
  , setCrontab
    -- * Data types
  , CrontabText (..)
  , CronEntry (..)
    -- * Pure helpers
  , parseCrontab
  , renderCrontab
  , formatCrontab
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Kind (Type)
import Polysemy (makeSem)

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

-- | Raw crontab content, as returned by @crontab -l@.
newtype CrontabText = CrontabText Text
  deriving stock (Show, Eq)

-- | A single parsed cron entry (non-comment, non-blank line).
data CronEntry = CronEntry
  { cronSchedule :: Text    -- ^ e.g. @"0 * * * *"@ or @"@daily"@
  , cronCommand  :: Text    -- ^ the command to run
  , cronComment  :: Maybe Text  -- ^ optional inline comment (after @#@)
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Effect
--------------------------------------------------------------------------------

-- | Operations on the user's crontab.
data Cron (m :: Type -> Type) a where
  ListCrontab :: Cron m CrontabText
  SetCrontab  :: CrontabText -> Cron m ()

makeSem ''Cron

--------------------------------------------------------------------------------
-- Pure helpers (no effect required)
--------------------------------------------------------------------------------

-- | Parse raw crontab text into structured entries.
-- Lines starting with @#@ or blank lines are skipped.
-- Entries with fewer than 6 whitespace-separated tokens are also skipped.
parseCrontab :: CrontabText -> [CronEntry]
parseCrontab (CrontabText raw) =
  [ parseLine line
  | line <- T.lines raw
  , not (T.null (T.strip line))
  , not ("#" `T.isPrefixOf` T.strip line)
  ]
  where
    parseLine :: Text -> CronEntry
    parseLine line =
      let tokens = T.words line
          -- Cron schedule is 5 tokens (min hr dom mon dow) or a @keyword
          (schedTokens, rest) = splitSchedule tokens
          schedule = T.unwords schedTokens
          (cmd, comment) = splitComment (T.unwords rest)
      in CronEntry schedule cmd comment

    -- @keyword lines have 1 schedule token; standard lines have 5
    splitSchedule :: [Text] -> ([Text], [Text])
    splitSchedule [] = ([], [])
    splitSchedule ts@(t:_)
      | "@" `T.isPrefixOf` t = ([t], drop 1 ts)
      | otherwise             = splitAt 5 ts

    -- Split command from trailing inline comment
    splitComment :: Text -> (Text, Maybe Text)
    splitComment t =
      case T.breakOn " #" t of
        (cmd, "") -> (T.strip cmd, Nothing)
        (cmd, comment) ->
          let c = T.strip (T.drop 2 comment)
          in (T.strip cmd, if T.null c then Nothing else Just c)

-- | Render a list of 'CronEntry' values back to crontab text.
renderCrontab :: [CronEntry] -> CrontabText
renderCrontab entries = CrontabText $ T.unlines $ map renderEntry entries
  where
    renderEntry (CronEntry sched cmd Nothing)      = sched <> " " <> cmd
    renderEntry (CronEntry sched cmd (Just cmt)) = sched <> " " <> cmd <> " # " <> cmt

-- | Format crontab text with line numbers for display.
formatCrontab :: CrontabText -> Text
formatCrontab (CrontabText raw) =
  let ls = T.lines raw
  in T.unlines $ zipWith (\n l -> T.pack (show n) <> "\t" <> l) [(1::Int)..] ls
