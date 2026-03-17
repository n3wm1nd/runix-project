-- | HTML export for PromptTuner sessions via Mustache templates.
--
-- Accumulates Progress events into a session, then renders a self-contained
-- HTML report using either the built-in default template or a user-supplied
-- @.mustache@ file.
module Export.Html
  ( runProgressHtml
  , defaultTemplate
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Polysemy
import Polysemy.State

import Text.Mustache (compileTemplate, substitute, ToMustache(..))
import Text.Mustache.Types (object, (~>))

import Progress

--------------------------------------------------------------------------------
-- Session model + ToMustache instances
--------------------------------------------------------------------------------

data AttemptCtx = AttemptCtx
  { acIndex      :: Int
  , acScore      :: T.Text
  , acPitch      :: T.Text
  , acEval       :: T.Text
  , acRebuttal   :: T.Text
  , acReeval     :: T.Text
  , acSummary    :: T.Text
  , acReflection :: T.Text
  }

instance ToMustache AttemptCtx where
  toMustache a = object
    [ "index"      ~> acIndex a
    , "score"      ~> acScore a
    , "pitch"      ~> acPitch a
    , "eval"       ~> acEval a
    , "rebuttal"   ~> acRebuttal a
    , "reeval"     ~> acReeval a
    , "summary"    ~> acSummary a
    , "reflection" ~> acReflection a
    ]

data SessionCtx = SessionCtx
  { scIdea     :: T.Text
  , scOutcome  :: T.Text
  , scAttempts :: [AttemptCtx]
  }

instance ToMustache SessionCtx where
  toMustache s = object
    [ "idea"     ~> scIdea s
    , "outcome"  ~> scOutcome s
    , "attempts" ~> scAttempts s
    ]

--------------------------------------------------------------------------------
-- Accumulator
--------------------------------------------------------------------------------

data Acc = Acc
  { accIdea    :: T.Text
  , accDone    :: [AttemptCtx]  -- reversed
  , accCurrent :: AttemptCtx
  }

emptyAttempt :: Int -> AttemptCtx
emptyAttempt n = AttemptCtx n "" "" "" "" "" "" ""

showScore :: Maybe Int -> T.Text
showScore = maybe "?" (\n -> T.pack (show n) <> "/10")

modifyCurrent :: (AttemptCtx -> AttemptCtx) -> Acc -> Acc
modifyCurrent f a = a { accCurrent = f (accCurrent a) }

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

-- | Observe 'Progress' events and write an HTML report, without consuming the
-- effect.  Compose this before 'runProgressStdout' (or any other interpreter).
runProgressHtml
  :: Member (Embed IO) r
  => FilePath          -- ^ output file path
  -> Maybe FilePath    -- ^ optional custom template (must contain {{...}})
  -> Sem (Progress : r) a
  -> Sem r a
runProgressHtml outPath mTemplatePath =
  evalState (Acc "" [] (emptyAttempt 1))
  . interpret (\case
      ReportHeader idea   -> modify $ \a -> a { accIdea = idea }
      ReportAttempt n _   -> modify $ \a -> a { accCurrent = emptyAttempt n }
      ReportPitch p       -> modify $ modifyCurrent $ \c -> c { acPitch      = p  }
      ReportEval r        -> modify $ modifyCurrent $ \c -> c { acEval       = r  }
      ReportRebuttal rb   -> modify $ modifyCurrent $ \c -> c { acRebuttal   = rb }
      ReportReeval rv     -> modify $ modifyCurrent $ \c -> c { acReeval     = rv }
      ReportRating s v    -> modify $ modifyCurrent $ \c -> c { acScore = showScore s, acSummary = v }
      ReportReflect rf    -> modify $ modifyCurrent $ \c -> c { acReflection = rf }
      ReportSuccess n att -> flush $
        "Score " <> T.pack (show n) <> "/10 on attempt " <> T.pack (show att) <> " — SUCCESS"
      ReportGaveUp        -> flush "Gave up after reaching maximum attempts."
    )
  . raiseUnder
  where
    flush :: Members [State Acc, Embed IO] r' => T.Text -> Sem r' ()
    flush outcome = do
      acc <- get
      let session = SessionCtx
            { scIdea     = accIdea acc
            , scOutcome  = outcome
            , scAttempts = reverse (accCurrent acc : accDone acc)
            }
      embed $ do
        tmplSrc <- maybe (pure defaultTemplate) TIO.readFile mTemplatePath
        case compileTemplate "prompttuner" tmplSrc of
          Left err   -> putStrLn $ "HTML export: template error: " <> show err
          Right tmpl -> TIO.writeFile outPath (substitute tmpl session)

--------------------------------------------------------------------------------
-- Default template
--------------------------------------------------------------------------------

defaultTemplate :: T.Text
defaultTemplate = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>PromptTuner: {{idea}}</title>"
  , "  <style>"
  , "    body { font-family: system-ui, sans-serif; max-width: 860px;"
  , "           margin: 2rem auto; padding: 0 1rem; line-height: 1.6; }"
  , "    h1   { border-bottom: 2px solid #333; padding-bottom: .4rem; }"
  , "    h2   { margin-top: 2rem; }"
  , "    .attempt { border: 1px solid #ddd; border-radius: 6px;"
  , "               padding: 1rem; margin: 1rem 0; }"
  , "    details  { margin: .6rem 0; }"
  , "    summary  { cursor: pointer; user-select: none; }"
  , "    pre { white-space: pre-wrap; background: #f6f8fa;"
  , "          border-radius: 4px; padding: .6rem; margin: .4rem 0; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <h1>PromptTuner: {{idea}}</h1>"
  , "  <p><strong>{{outcome}}</strong></p>"
  , "  {{#attempts}}"
  , "  <div class=\"attempt\">"
  , "    <h2>Attempt {{index}} — {{score}}</h2>"
  , "    <details><summary><strong>Pitch</strong></summary><pre>{{pitch}}</pre></details>"
  , "    <details><summary><strong>Evaluation</strong></summary><pre>{{eval}}</pre></details>"
  , "    <details><summary><strong>Rebuttal</strong></summary><pre>{{rebuttal}}</pre></details>"
  , "    <details><summary><strong>Re-evaluation</strong></summary><pre>{{reeval}}</pre></details>"
  , "    <details><summary><strong>Verdict</strong></summary><pre>{{summary}}</pre></details>"
  , "    <details><summary><strong>Reflection</strong></summary><pre>{{reflection}}</pre></details>"
  , "  </div>"
  , "  {{/attempts}}"
  , "</body>"
  , "</html>"
  ]
