{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Runix.Secret.Effects (Secret, getSecret)
import Runix.LLM.Effects
import Runix.LLM.Interpreter (LLMConfig(..), interpretLLM)
import Runix.HTTP.Effects
import Runix.FileSystem.Effects
import Runix.Logging.Effects
import Runix.Runners.CLI.Chat (chatLoop)

import UniversalLLM.Core.Types (ModelName(..), ProviderImplementation(..), ModelConfig(..), Tool(..), LLMTool(..), llmToolToDefinition, executeToolCall, ToolCall(..), HasTools(..), Message(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), baseComposableProvider, openAIWithTools)
import UniversalLLM.Providers.XMLToolCalls (withXMLToolCalls)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import GHC.Generics (Generic)
import Data.Aeson ()  -- Import instances only
import Autodocodec

-- GLM4.5 model with XML-style tool calls (running on local llama.cpp server)
data GLM45 = GLM45 deriving (Show, Eq)

instance ModelName LlamaCpp GLM45 where
    modelName _ = "glm-4-9b-chat"

instance HasTools GLM45 LlamaCpp where
    -- llama-cpp handles tool definitions in system prompt, but returns XML
    -- So we use withXMLToolCalls to parse XML responses into proper tool calls
    withTools = withXMLToolCalls . UniversalLLM.Providers.OpenAI.openAIWithTools

instance ProviderImplementation LlamaCpp GLM45 where
    getComposableProvider = withTools $ UniversalLLM.Providers.OpenAI.baseComposableProvider

instance Coding GLM45

-- Logging tool parameters with automatic schema generation
data LoggingToolParams = LoggingToolParams
    { logMessage :: T.Text
    , logLevel :: T.Text  -- "info", "warning", "error"
    } deriving (Show, Eq, Generic)

-- AutoDoCodec instance for automatic schema generation
instance HasCodec LoggingToolParams where
    codec = object "LoggingToolParams" $
        LoggingToolParams
            <$> requiredField "message" "The message to log" .= logMessage
            <*> requiredField "level" "Log level: info, warning, or error" .= logLevel

-- Logging tool result
data LoggingToolResult = LoggingToolResult
    { success :: Bool
    , message :: T.Text
    } deriving (Show, Eq, Generic)

instance HasCodec LoggingToolResult where
    codec = object "LoggingToolResult" $
        LoggingToolResult
            <$> requiredField "success" "Whether the logging succeeded" .= success
            <*> requiredField "message" "Result message" .= message

-- Logging tool type - allows LLM to log messages
data LoggingTool m = LoggingTool (LoggingToolParams -> m LoggingToolResult)

instance Tool (LoggingTool m) m where
    type ToolParams (LoggingTool m) = LoggingToolParams
    type ToolOutput (LoggingTool m) = LoggingToolResult

    toolName _ = "log_message"
    toolDescription _ = "Log a message to the system with specified level (info, warning, or error)"

    call (LoggingTool impl) params = impl params

-- Create the logging tool with actual implementation that uses the Logging effect
loggingTool :: forall r. Members '[Logging] r => LoggingTool (Sem r)
loggingTool = LoggingTool $ \params -> do
    let msg = logMessage params
    let lvl = logLevel params
    case lvl of
        "info" -> info msg
        "warning" -> warning msg
        "error" -> error msg
        _ -> warning $ "Unknown log level: " <> lvl <> ", message: " <> msg
    return $ LoggingToolResult True ("Logged message at level: " <> lvl)

-- Our custom SafeEffects type with LlamaCpp LLM
type ChatbotEffects = [FileSystem, HTTP, Logging, LLM LlamaCpp GLM45, Fail, Embed IO]

-- Type for the API key secret
newtype LlamaCppApiKey = LlamaCppApiKey String

-- Setup LlamaCpp LLM with environment endpoint
llamaCppLLM :: Members '[Embed IO, Fail, HTTP, Logging, Secret LlamaCppApiKey] r => Sem (LLM LlamaCpp GLM45 : r) a -> Sem r a
llamaCppLLM action = do
    endpoint <- embed $ lookupEnv "OPENAI_ENDPOINT"
    LlamaCppApiKey apiKey <- getSecret
    case endpoint of
        Nothing -> fail "OPENAI_ENDPOINT environment variable is not set"
        Just ep -> do
            let config = LLMConfig
                    { llmProvider = LlamaCpp
                    , llmEndpoint = ep <> "/v1/chat/completions"
                    , llmHeaders = [("Authorization", "Bearer " <> apiKey), ("Content-Type", "application/json")]
                    }
            interpretLLM config GLM45 action

-- Our custom run stack (copying runUntrusted structure)
runChatbot :: HasCallStack => (forall r . Members ChatbotEffects r => Sem r a) -> IO (Either String a)
runChatbot = runM . runError . loggingIO . failLog . httpIO (withRequestTimeout 300) . filesystemIO . secretEnv LlamaCppApiKey "OPENAI_API_KEY" . llamaCppLLM

-- Extract text and tool calls from assistant messages
extractFromMessages :: [Message GLM45 LlamaCpp] -> ([T.Text], [ToolCall])
extractFromMessages msgs = foldr processMessage ([], []) msgs
  where
    processMessage (AssistantText txt) (texts, calls) = (txt:texts, calls)
    processMessage (AssistantTool toolCall) (texts, calls) = (texts, toolCall:calls)
    processMessage (AssistantReasoning txt) (texts, calls) = (("[Thinking: " <> txt <> "]"):texts, calls)
    processMessage _ acc = acc

-- Pure display helper
displayTexts :: Members ChatbotEffects r => [T.Text] -> Sem r ()
displayTexts texts = mapM_ (embed . T.putStrLn) texts

-- Chatbot agent - updated for new LLM effect
chatbotAgent :: forall r. Members ChatbotEffects r => T.Text -> [Message GLM45 LlamaCpp] -> Sem r [Message GLM45 LlamaCpp]
chatbotAgent userInput history = do
    info $ "User input: " <> userInput

    -- Get tool list
    let tools :: [LLMTool (Sem r)]
        tools = [LLMTool (loggingTool @r)]

    -- Build config with tools
    let toolDefs = map llmToolToDefinition tools
        configs = [ Temperature 0.7
                  , MaxTokens 500
                  , Tools toolDefs
                  , SystemPrompt "You are a helpful assistant. Be friendly and concise."
                  ]

    -- Add user message and query
    let newHistory = history ++ [UserText userInput]
    responseMsgs <- queryLLM @LlamaCpp @GLM45 configs newHistory

    -- Process response
    handleResponse tools (newHistory ++ responseMsgs) responseMsgs

-- Handle responses recursively - execute tools if needed
handleResponse :: Members ChatbotEffects r => [LLMTool (Sem r)] -> [Message GLM45 LlamaCpp] -> [Message GLM45 LlamaCpp] -> Sem r [Message GLM45 LlamaCpp]
handleResponse tools history responseMsgs = do
    let (texts, toolCalls) = extractFromMessages responseMsgs
    displayTexts texts

    case toolCalls of
        [] -> return history
        calls -> do
            info $ "Executing " <> T.pack (show (length calls)) <> " tool calls..."

            -- Execute all tool calls
            toolResults <- mapM (executeToolCall tools) calls

            -- Add tool results to history and query again
            let toolResultMsgs = map ToolResultMsg toolResults
                newHistory = history ++ toolResultMsgs

            -- Get tool configs again
            let toolDefs = map llmToolToDefinition tools
                configs = [ Temperature 0.7
                          , MaxTokens 500
                          , Tools toolDefs
                          , SystemPrompt "You are a helpful assistant. Be friendly and concise."
                          ]

            responseMsgs' <- queryLLM @LlamaCpp @GLM45 configs newHistory
            handleResponse tools (newHistory ++ responseMsgs') responseMsgs'

main :: IO ()
main = do
    putStrLn "Starting Runix chatbot with GLM-4.5 (XML tool calls). Type your messages (Ctrl+D to exit):"
    result <- runChatbot (chatLoop chatbotAgent ([] :: [Message GLM45 LlamaCpp]))
    case result of
        Right () -> return ()
        Left errMsg -> hPutStr stderr errMsg >> exitFailure
