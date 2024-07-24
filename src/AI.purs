module AI where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Foreign.Object (Object)
import Partial.Unsafe (unsafeCrashWith)

-- =============================================================================
-- Client 

foreign import data Client :: Type

foreign import make_client :: { token :: String } -> Effect Client

-- =============================================================================
-- Chat

chat :: Client -> ChatRequest -> Aff NonStreamedChatResponse
chat client request = toAffE (chat_ { client, request })

foreign import chat_ :: { client :: Client, request :: ChatRequest } -> Effect (Promise NonStreamedChatResponse)

type ChatRequest =
  { message :: String
  , model :: String
  , preamble :: String
  , chatHistory :: Array Message
  -- , conversationId :: String
  -- , promptTruncation :: Cohere.ChatRequestPromptTruncation
  -- , connectors :: Array Cohere.ChatConnector
  -- , searchQueriesOnly :: Boolean
  -- , documents :: Array Cohere.ChatDocument
  -- , citationQuality :: Cohere.ChatRequestCitationQuality
  -- , temperature :: Number
  -- , maxTokens :: Int
  -- , maxInputTokens :: Int
  -- , k :: Number
  -- , p :: Number
  -- , seed :: Number
  -- , stopSequences :: Array String
  -- , frequencyPenalty :: Number
  -- , presencePenalty :: Number
  -- , rawPrompting :: Boolean
  -- , returnPrompt :: Boolean
  , tools :: Tools
  -- , toolResults :: Array Cohere.ToolResult
  , forceSingleStep :: Boolean
  }

type NonStreamedChatResponse =
  { text :: String
  -- , generationId :: String
  -- , citations :: Array Cohere.ChatCitation
  -- , documents :: Array Cohere.ChatDocument
  -- , isSearchRequired :: Boolean
  -- , searchQueries :: Array Cohere.ChatSearchQuery
  -- , searchResults :: Array Cohere.ChatSearchResult
  -- , finishReason :: Cohere.FinishReason
  , toolCalls :: Array ToolCall
  , chatHistory :: Array Message
  -- , prompt :: String
  -- , meta :: Cohere.ApiMeta
  }

type Message =
  { role :: MessageRole
  , message :: String
  , toolCalls :: Array ToolCall
  }

data MessageRole = CHATBOT | SYSTEM | USER | TOOL

derive instance Generic MessageRole _

instance Show MessageRole where
  show x = genericShow x

instance Eq MessageRole where
  eq x = genericEq x

instance EncodeJson MessageRole where
  encodeJson = show >>> encodeJson

instance DecodeJson MessageRole where
  decodeJson json = do
    str :: String <- decodeJson json
    Array.find ((str == _) <<< show) [ CHATBOT, SYSTEM, USER, TOOL ] # maybe (throwError (UnexpectedValue json)) pure

--  >>= case _ of
-- str | str == show CHATBOT -> pure CHATBOT
-- _ -> throwError (UnexpectedValue json)

-- =============================================================================
-- Tools

type Tools = Array Tool

type Tool =
  { name :: String
  , description :: String
  , parameterDefinitions :: ToolParameterDefinitionsValues
  }

type ToolParameterDefinitionsValues = Object ToolParameterDefinitionsValue

type ToolParameterDefinitionsValue =
  { description :: String
  , type :: String
  , required :: Boolean
  }

-- =============================================================================
-- ToolCalls

type ToolCalls = Array ToolCall

type ToolCall =
  { name :: String
  , parameters :: ToolCallParameters
  }

type ToolCallParameters = Json

data Any
