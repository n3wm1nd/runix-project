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
{-# LANGUAGE DerivingVia #-}

module Main where

import Prelude hiding (readFile, writeFile, error)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)
import GHC.IO.Handle (hPutStr)

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Runner (Coding)
import Runix.LLM.Effects
import Runix.LLM.Interpreter
import Runix.LLM.ToolInstances ()  -- Import orphan instances
import Runix.HTTP.Effects
import Runix.FileSystem.Simple.Effects (FileSystemRead, FileSystemWrite, filesystemIO)
import Runix.Logging.Effects
import Runix.Cancellation.Effects (Cancellation, cancelNoop)
import Runix.Runners.CLI.Chat (chatLoop)

import UniversalLLM.Core.Tools
import UniversalLLM.Providers.XMLToolCalls (xmlResponseParser)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import GHC.Generics (Generic)
import Data.Aeson ()  -- Import instances only
import Autodocodec
import qualified UniversalLLM.Providers.OpenAI
import UniversalLLM.Protocols.OpenAI (OpenAIRequest, OpenAIResponse)
import qualified Data.ByteString as BS
import Runix.Streaming.Effects (StreamChunk, ignoreChunks)

-- No ChatProvider/ChatModel wrappers - business logic is fully polymorphic
    

-- GLM4.5 model with XML-style tool calls (running on local llama.cpp server)
data GLM45 = GLM45 deriving (Show, Eq)
instance Provider (Model GLM45 LlamaCpp) where
    type ProviderRequest (Model GLM45 LlamaCpp) = OpenAIRequest
    type ProviderResponse (Model GLM45 LlamaCpp) = OpenAIResponse

instance ModelName (Model GLM45 LlamaCpp) where
    modelName (Model _ _) = "glm-4-9b-chat"

instance HasTools (Model GLM45 LlamaCpp) where
    -- llama-cpp handles tool definitions in system prompt, but returns XML
    -- We use openAI tools for request building, and XML parsing for responses
    withTools = xmlResponseParser

-- Composable provider for GLM45
-- Note: withXMLResponseParsing handles XML parsing, but we still need openAI tool definitions
-- Those come from the system prompt configuration in chatbotAgent
glm45ComposableProvider :: ComposableProvider (Model GLM45 LlamaCpp) (ToolState (Model GLM45 LlamaCpp), ())
glm45ComposableProvider = withTools `chainProviders` UniversalLLM.Providers.OpenAI.baseComposableProvider

instance Coding GLM45

-- Newtype wrappers for tool parameters to give them proper names
newtype LogMessage = LogMessage T.Text deriving (Show, Eq, HasCodec) via T.Text
newtype LogLevel = LogLevel T.Text deriving (Show, Eq, HasCodec) via T.Text

instance ToolParameter LogMessage where
    paramName _ _ = "message"
    paramDescription _ = "The message to log"

instance ToolParameter LogLevel where
    paramName _ _ = "level"
    paramDescription _ = "Log level: info, warning, or error"

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

-- Make LoggingToolResult a ToolParameter for use in multi-parameter functions
instance ToolParameter LoggingToolResult where
    paramName _ n = "logging_result_" <> T.pack (show n)
    paramDescription _ = "logging operation result"

-- Make LoggingToolResult a ToolFunction so functions returning it become tools automatically
instance ToolFunction LoggingToolResult where
    toolFunctionName _ = "log_message"
    toolFunctionDescription _ = "Log a message to the system with specified level (info, warning, or error)"

-- Create the logging tool as a bare function
loggingToolFunc :: Members '[Logging] r => LogMessage -> LogLevel -> Sem r LoggingToolResult
loggingToolFunc (LogMessage msg) (LogLevel lvl) = do
    case lvl of
        "info" -> info msg
        "warning" -> warning msg
        "error" -> error msg
        _ -> warning $ "Unknown log level: " <> lvl <> ", message: " <> msg
    return $ LoggingToolResult True ("Logged message at level: " <> lvl)

-- Setup LlamaCpp LLM with environment endpoint (only place we specify concrete types)
llamaCppLLM :: forall r a. Members '[Embed IO, Fail, HTTP, HTTPStreaming, Logging, Cancellation] r
            => Sem (LLM (Model GLM45 LlamaCpp) : r) a
            -> Sem r a
llamaCppLLM action = do
    endpoint <- embed $ lookupEnv "OPENAI_ENDPOINT"
    case endpoint of
        Nothing -> fail "OPENAI_ENDPOINT environment variable is not set"
        Just ep -> interpretLlamaCpp glm45ComposableProvider ep (Model GLM45 LlamaCpp) action

-- Our custom run stack (copying runUntrusted structure)
runChatbot :: HasCallStack
           => (forall r. Members '[FileSystemRead, FileSystemWrite, HTTP, HTTPStreaming, StreamChunk BS.ByteString, Logging, LLM (Model GLM45 LlamaCpp), Fail, Embed IO, Cancellation] r => Sem r a)
           -> IO (Either String a)
runChatbot = runM . runError . loggingIO . failLog . cancelNoop . ignoreChunks @BS.ByteString . httpIOStreaming (withRequestTimeout 300) . httpIO (withRequestTimeout 300) . filesystemIO . llamaCppLLM

-- Extract text and tool calls from assistant messages - pure function
extractFromMessages :: [Message model] -> ([T.Text], [ToolCall])
extractFromMessages msgs = foldr processMessage ([], []) msgs
  where
    processMessage (AssistantText txt) (texts, calls) = (txt:texts, calls)
    processMessage (AssistantTool toolCall) (texts, calls) = (texts, toolCall:calls)
    processMessage (AssistantReasoning txt) (texts, calls) = (("[Thinking: " <> txt <> "]"):texts, calls)
    processMessage _ acc = acc

-- Pure display helper - only needs IO
displayTexts :: Member (Embed IO) r => [T.Text] -> Sem r ()
displayTexts texts = mapM_ (embed . T.putStrLn) texts

-- Chatbot agent - polymorphic over model
chatbotAgent :: forall model r.
                ( Member (LLM model) r
                , Member Logging r
                , Member (Embed IO) r
                , HasTools model
                , SupportsTemperature (ProviderOf model)
                , SupportsMaxTokens (ProviderOf model)
                , SupportsSystemPrompt (ProviderOf model)
                )
             => T.Text
             -> [Message model]
             -> Sem r [Message model]
chatbotAgent userInput history = do
    info $ "User input: " <> userInput

    -- Get tool list - using ToolFunction instance (cleanest approach)
    let tools :: [LLMTool (Sem r)]
        tools = [LLMTool (loggingToolFunc @r)]

    -- Build config with tools
    let toolDefs = map llmToolToDefinition tools
        configs = [ Temperature 0.7
                  , MaxTokens 500
                  , Tools toolDefs
                  , SystemPrompt "You are a helpful assistant. Be friendly and concise."
                  ]

    -- Add user message and query
    let newHistory = history ++ [UserText userInput]
    responseMsgs <- queryLLM @model configs newHistory

    -- Process response
    handleResponse tools (newHistory ++ responseMsgs) responseMsgs

-- Handle responses recursively - execute tools if needed - polymorphic
handleResponse :: forall model r.
                  ( Member (LLM model) r
                  , Member Logging r
                  , Member (Embed IO) r
                  , HasTools model
                  , SupportsTemperature (ProviderOf model)
                  , SupportsMaxTokens (ProviderOf model)
                  , SupportsSystemPrompt (ProviderOf model)
                  )
               => [LLMTool (Sem r)]
               -> [Message model]
               -> [Message model]
               -> Sem r [Message model]
handleResponse tools history responseMsgs = do
    let (texts, toolCalls) = extractFromMessages responseMsgs
    displayTexts texts

    case toolCalls of
        [] -> return history
        calls -> do
            info $ "Executing " <> T.pack (show (length calls)) <> " tool calls..."

            -- Execute all tool calls
            toolResults <- mapM (executeToolCallFromList tools) calls

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

            responseMsgs' <- queryLLM @model configs newHistory
            handleResponse tools (newHistory ++ responseMsgs') responseMsgs'

main :: IO ()
main = do
    putStrLn "Starting Runix chatbot with GLM-4.5 (XML tool calls). Type your messages (Ctrl+D to exit):"
    result <- runChatbot (chatLoop chatbotAgent ([] :: [Message (Model GLM45 LlamaCpp)]))
    case result of
        Right () -> return ()
        Left errMsg -> hPutStr stderr errMsg >> exitFailure
