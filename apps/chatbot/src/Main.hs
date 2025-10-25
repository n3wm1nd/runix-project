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
import Control.Monad (unless)

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Runner (filesystemIO, httpIO, withRequestTimeout, secretEnv, loggingIO, failLog, Coding)
import Runix.LLM.Effects
import Runix.LLM.OpenAICompatible
import Runix.LLM.Protocol.OpenAICompatible
import Runix.RestAPI.Effects
import Runix.HTTP.Effects
import Runix.FileSystem.Effects
import Runix.Logging.Effects
import Runix.Runners.CLI.Chat (chatLoop)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, Value)
import qualified Data.Aeson as Aeson
import Autodocodec
import Autodocodec.Schema
import Data.Aeson.Types (emptyObject)
import qualified Autodocodec as ADC

-- GLM3.5 reasoning model with tools support
data GLM35 = GLM35
    { maxTokens :: Maybe Int
    , reasoning :: Maybe ReasoningConfig
    , tools :: [ToolDefinition]
    , systemPrompt :: Maybe T.Text
    }

instance OpenAICompatibleModel GLM35 where
    openaiCompatibleModelId _ = "GLM3.5"
    openaiCompatibleSetParameters model query = query
        { max_tokens = model.maxTokens
        , reasoning = model.reasoning
        , tools = Just model.tools
        }

instance HasSystemPrompt GLM35 where
    getSystemPrompt = systemPrompt

instance Coding GLM35

-- Logging tool parameters with automatic schema generation
data LoggingToolParams = LoggingToolParams
    { logMessage :: T.Text
    , logLevel :: T.Text  -- "info", "warning", "error"
    } deriving (Show, Eq, Generic)

-- AutoDoCodec instance for automatic schema generation
instance HasCodec LoggingToolParams where
    codec = ADC.object "LoggingToolParams" $
        LoggingToolParams
            <$> requiredField "message" "The message to log" ADC..= logMessage
            <*> requiredField "level" "Log level: info, warning, or error" ADC..= logLevel

-- Manual FromJSON/ToJSON instances using the codec
instance FromJSON LoggingToolParams where
    parseJSON = parseJSONViaCodec

instance ToJSON LoggingToolParams where
    toJSON = toJSONViaCodec

-- Automatic tool definition generation from codec
loggingTool :: ToolDefinition
loggingTool = ToolDefinition
    { toolType = "function"
    , function = FunctionDefinition
        { name = "log_message"
        , description = "Log a message to the system with specified level"
        , parameters = Aeson.toJSON $ jsonSchemaViaCodec @LoggingToolParams
        }
    }

-- Our custom SafeEffects type with OpenAI compatible LLM
type ChatbotEffects = [FileSystem, HTTP, Logging, LLM GLM35, Fail, Embed IO]

-- Setup OpenAI Compatible LLM with environment endpoint
openaiCompatibleLLM :: Members [Embed IO, Fail, HTTP, Logging] r => Sem (LLM GLM35 : RestAPI OpenAICompatible : r) a -> Sem r a
openaiCompatibleLLM action = do
    endpoint <- embed $ lookupEnv "OPENAI_ENDPOINT"
    case endpoint of
        Nothing -> fail "OPENAI_ENDPOINT environment variable is not set"
        Just ep -> secretEnv OpenAICompatibleKey "OPENAI_API_KEY" $
            openaiCompatibleAPI ep $
            llmOpenAICompatible (GLM35 Nothing (Just (ReasoningConfig (Just 100) (Just "low"))) [loggingTool] (Just "You are a helpful assistant. Be friendly and concise.")) action

-- Our custom run stack (copying runUntrusted structure)
runChatbot :: HasCallStack => (forall r . Members ChatbotEffects r => Sem r a) -> IO (Either String a)
runChatbot = runM . runError . loggingIO . failLog . httpIO (withRequestTimeout 300) . filesystemIO . openaiCompatibleLLM

-- Pure logic: process assistant response and determine what to display/execute
processAssistantResponse :: AssistantOutput -> ([T.Text], [ToolCall])
processAssistantResponse (AssistantResponse responseText toolCalls) =
    let (cleanContent, thinking) = Runix.LLM.Effects.extractThinking responseText
        contentTexts = [cleanContent | not (T.null cleanContent)]
        thinkingTexts = ["[Thinking: " <> t <> "]" | Just t <- [thinking]]
        toolCallText = ["Executing " <> T.pack (show (length toolCalls)) <> " tool calls..." | not (null toolCalls)]
    in (contentTexts ++ thinkingTexts ++ toolCallText, toolCalls)

-- Pure display helper
displayTexts :: Members ChatbotEffects r => [T.Text] -> Sem r ()
displayTexts texts = mapM_ (embed . T.putStrLn) texts

-- Chatbot agent - now much cleaner
chatbotAgent :: Members ChatbotEffects r => T.Text -> MessageHistory -> Sem r MessageHistory
chatbotAgent userInput history = do
    info $ "User input: " <> userInput
    model <- getModel @GLM35
    (newHistory, assistantOutput) <- queryLLM model history (UserQuery userInput)
    handleResponse model newHistory assistantOutput

-- Handle responses recursively - pure logic separated from effects
handleResponse :: Members ChatbotEffects r => GLM35 -> MessageHistory -> AssistantOutput -> Sem r MessageHistory
handleResponse model history response = do
    let (textsToDisplay, toolCalls) = processAssistantResponse response
    displayTexts textsToDisplay

    case toolCalls of
        [] -> return history
        calls -> do
            toolResults <- mapM executeToolCall calls
            let results = zipWith (\call result -> ToolResult result (toolId call)) calls toolResults
            (newHistory, newResponse) <- queryLLM model history (ToolCallResults results)
            handleResponse model newHistory newResponse

-- Execute a single tool call
executeToolCall :: Members ChatbotEffects r => ToolCall -> Sem r T.Text
executeToolCall (ToolCall toolName toolId params) = do
    case toolName of
        "log_message" -> do
            case eitherDecode (encode params) of
                Right params' -> do
                    let msg = logMessage params'
                    let lvl = logLevel params'
                    case lvl of
                        "info" -> info msg
                        "warning" -> warning msg
                        "error" -> error msg
                        _ -> warning $ "Unknown log level: " <> lvl <> ", message: " <> msg
                    return $ "Logged message: " <> msg <> " at level: " <> lvl
                Left err -> do
                    warning $ "Failed to parse tool parameters: " <> T.pack err
                    return $ "Error: Failed to parse tool parameters"
        _ -> do
            warning $ "Unknown tool: " <> toolName
            return $ "Error: Unknown tool " <> toolName

main :: IO ()
main = do
    putStrLn "Starting Runix chatbot. Type your messages (Ctrl+D to exit):"
    result <- runChatbot (chatLoop chatbotAgent ([] :: MessageHistory))
    case result of
        Right () -> return ()
        Left errMsg -> hPutStr stderr errMsg >> exitFailure
