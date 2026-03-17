{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Progress
  ( Progress(..)
  , reportHeader
  , reportAttempt
  , reportPitch
  , reportEval
  , reportRebuttal
  , reportReeval
  , reportRating
  , reportReflect
  , reportSuccess
  , reportGaveUp
  , runProgressStdout
  ) where

import Data.Kind (Type)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Polysemy

data Progress (m :: Type -> Type) a where
  ReportHeader   :: T.Text -> Progress m ()
  ReportAttempt  :: Int -> Int -> Progress m ()
  ReportPitch    :: T.Text -> Progress m ()
  ReportEval     :: T.Text -> Progress m ()
  ReportRebuttal :: T.Text -> Progress m ()
  ReportReeval   :: T.Text -> Progress m ()
  ReportRating   :: Maybe Int -> T.Text -> Progress m ()
  ReportReflect  :: T.Text -> Progress m ()
  ReportSuccess  :: Int -> Int -> Progress m ()
  ReportGaveUp   :: Progress m ()

makeSem ''Progress

-- | Interpreter that prints all progress to stdout.
runProgressStdout :: Member (Embed IO) r => Sem (Progress : r) a -> Sem r a
runProgressStdout = interpret $ \case
  ReportHeader idea   -> embed $ TIO.putStrLn ("=== PromptTuner: " <> idea) >> TIO.putStrLn ""
  ReportAttempt n tot -> embed $ TIO.putStrLn ("--- Attempt " <> T.pack (show n) <> " / " <> T.pack (show tot) <> " ---")
  ReportPitch p       -> embed $ TIO.putStrLn "[PITCH]" >> TIO.putStrLn p >> TIO.putStrLn ""
  ReportEval r        -> embed $ TIO.putStrLn "[EVALUATION]" >> TIO.putStrLn r >> TIO.putStrLn ""
  ReportRebuttal rb   -> embed $ TIO.putStrLn "[REBUTTAL]" >> TIO.putStrLn rb >> TIO.putStrLn ""
  ReportReeval rv     -> embed $ TIO.putStrLn "[RE-EVALUATION]" >> TIO.putStrLn rv
  ReportRating s v    -> embed $ do
    TIO.putStrLn $ "[RATING] " <> maybe "?" (T.pack . show) s <> "/10"
    TIO.putStrLn $ "[SUMMARY] " <> v
    TIO.putStrLn ""
  ReportReflect rf    -> embed $ TIO.putStrLn "[REFLECTION]" >> TIO.putStrLn rf >> TIO.putStrLn ""
  ReportSuccess n att -> embed $ TIO.putStrLn $
    "[ SUCCESS ] Score " <> T.pack (show n) <> "/10 on attempt " <> T.pack (show att) <> "!"
  ReportGaveUp        -> embed $ TIO.putStrLn
    "[ GAVE UP ] Reached maximum attempts without convincing the evaluator."
