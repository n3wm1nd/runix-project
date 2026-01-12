{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Prelude hiding (readFile, writeFile, error)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)
import GHC.IO.Handle (hPutStr)

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Runner (filesystemIO, httpIO, withRequestTimeout, secretEnv, loggingIO, failLog, Coding)
import Runix.LLM
import Runix.LLM.OpenAICompatible
import Runix.LLM.Protocol.OpenAICompatible
import Runix.RestAPI
import Runix.HTTP
import Runix.FileSystem
import Runix.Logging

import qualified Data.Text as T
import GHC.Stack
import GHC.Generics

-- GLM3.5 reasoning model
data GLM35 = GLM35
    { maxTokens :: Maybe Int
    , reasoning :: Maybe ReasoningConfig
    }

instance OpenAICompatibleModel GLM35 where
    openaiCompatibleModelId _ = "GLM3.5"
    openaiCompatibleSetParameters model query = query
        { max_tokens = model.maxTokens
        , reasoning = model.reasoning
        }

instance Coding GLM35

-- Our custom SafeEffects type with OpenAI compatible LLM
type HelloWorldEffects = [FileSystem, HTTP, Logging, LLM GLM35, Fail]

-- Setup OpenAI Compatible LLM with environment endpoint
openaiCompatibleLLM :: Members [Embed IO, Fail, HTTP, Logging] r => Sem (LLM GLM35 : RestAPI OpenAICompatible : r) a -> Sem r a
openaiCompatibleLLM action = do
    endpoint <- embed $ lookupEnv "OPENAI_ENDPOINT"
    case endpoint of
        Nothing -> fail "OPENAI_ENDPOINT environment variable is not set"
        Just ep -> secretEnv OpenAICompatibleKey "OPENAI_API_KEY" $
            openaiCompatibleAPI ep $
            llmOpenAICompatible (GLM35 Nothing (Just (ReasoningConfig (Just 100) (Just "low")))) action

-- Our custom run stack (copying runUntrusted structure)
runTask :: HasCallStack => (forall r . Members HelloWorldEffects r => Sem r a) -> IO (Either String a)
runTask = runM . runError . loggingIO . failLog . httpIO (withRequestTimeout 300) . filesystemIO . openaiCompatibleLLM

-- Hello world task that makes an LLM query
helloWorldTask :: Members HelloWorldEffects r => Sem r T.Text
helloWorldTask = do
    info "Starting HelloWorld LLM task"
    response <- askLLM @GLM35 "Say hello world in a creative way. Be brief - just one short creative greeting, no explanations. /nothink"
    info $ "LLM response: " <> response
    return response

main :: IO ()
main = do
    result <- runTask helloWorldTask
    case result of
        Right output -> putStrLn $ T.unpack output
        Left errMsg -> hPutStr stderr errMsg >> exitFailure
