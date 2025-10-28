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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Prelude hiding (readFile, writeFile, error)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)
import GHC.IO.Handle (hPutStr)

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Runner (filesystemIO, httpIO, withRequestTimeout, loggingIO, failLog, Coding)
import Runix.LLM.Effects
import Runix.LLM.Interpreter 
import Runix.HTTP.Effects
import Runix.FileSystem.Effects
import Runix.Logging.Effects
import Runix.Runners.CLI.Chat (chatLoop)

import UniversalLLM.Providers.XMLToolCalls (withXMLToolCalls)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import GHC.Generics (Generic)
import Data.Aeson ()  -- Import instances only
import Autodocodec
import qualified UniversalLLM.Providers.OpenAI

-- No ChatProvider/ChatModel wrappers - business logic is fully polymorphic
    

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

-- Setup LlamaCpp LLM with environment endpoint (only place we specify concrete types)
llamaCppLLM :: forall r a. Members '[Embed IO, Fail, HTTP, Logging] r
            => Sem (LLM LlamaCpp GLM45 : r) a
            -> Sem r a
llamaCppLLM action = do
    endpoint <- embed $ lookupEnv "OPENAI_ENDPOINT"
    case endpoint of
        Nothing -> fail "OPENAI_ENDPOINT environment variable is not set"
        Just ep -> interpretLlamaCpp ep LlamaCpp GLM45 action

-- Our custom run stack (copying runUntrusted structure)
runChatbot :: HasCallStack
           => (forall r. Members '[FileSystem, HTTP, Logging, LLM LlamaCpp GLM45, Fail, Embed IO] r => Sem r a)
           -> IO (Either String a)
runChatbot = runM . runError . loggingIO . failLog . httpIO (withRequestTimeout 300) . filesystemIO . llamaCppLLM

-- Extract text and tool calls from assistant messages - pure function
extractFromMessages :: [Message model provider] -> ([T.Text], [ToolCall])
extractFromMessages msgs = foldr processMessage ([], []) msgs
  where
    processMessage (AssistantText txt) (texts, calls) = (txt:texts, calls)
    processMessage (AssistantTool toolCall) (texts, calls) = (texts, toolCall:calls)
    processMessage (AssistantReasoning txt) (texts, calls) = (("[Thinking: " <> txt <> "]"):texts, calls)
    processMessage _ acc = acc

-- Pure display helper - only needs IO
displayTexts :: Member (Embed IO) r => [T.Text] -> Sem r ()
displayTexts texts = mapM_ (embed . T.putStrLn) texts

-- Chatbot agent - polymorphic over provider and model
chatbotAgent :: forall provider model r.
                ( Member (LLM provider model) r
                , Member Logging r
                , Member (Embed IO) r
                , HasTools model provider
                , SupportsTemperature provider
                , SupportsMaxTokens provider
                , SupportsSystemPrompt provider
                )
             => T.Text
             -> [Message model provider]
             -> Sem r [Message model provider]
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
    responseMsgs <- queryLLM @provider @model configs newHistory

    -- Process response
    handleResponse tools (newHistory ++ responseMsgs) responseMsgs

-- Handle responses recursively - execute tools if needed - polymorphic
handleResponse :: forall provider model r.
                  ( Member (LLM provider model) r
                  , Member Logging r
                  , Member (Embed IO) r
                  , HasTools model provider
                  , SupportsTemperature provider
                  , SupportsMaxTokens provider
                  , SupportsSystemPrompt provider
                  )
               => [LLMTool (Sem r)]
               -> [Message model provider]
               -> [Message model provider]
               -> Sem r [Message model provider]
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

            responseMsgs' <- queryLLM @provider @model configs newHistory
            handleResponse tools (newHistory ++ responseMsgs') responseMsgs'

main :: IO ()
main = do
    putStrLn "Starting Runix chatbot with GLM-4.5 (XML tool calls). Type your messages (Ctrl+D to exit):"
    result <- runChatbot (chatLoop chatbotAgent ([] :: [Message GLM45 LlamaCpp]))
    case result of
        Right () -> return ()
        Left errMsg -> hPutStr stderr errMsg >> exitFailure
