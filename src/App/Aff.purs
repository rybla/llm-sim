module App.Aff where

import Prelude

import AI as Ai
import Common as C
import Control.Bind (bindFlipped)
import Control.Monad.Except (runExceptT, throwError)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.String as CodePoints
import Data.String as String
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Data.Variant as V
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Secret as Secret
import Type.Proxy (Proxy(..))

runModel
  :: forall state actionsSpec actions
   . C.ActionsSpec actionsSpec actions
  => C.Model state actionsSpec actions
  -> Aff Unit
runModel (C.Model model) = do
  let
    sequentialize_actionsSpec =
      { submit_interpretation: C.ActionSpec
          { description: ""
          , params:
              { actions:
                  C.ActionParamSpec
                    { description: "The sequence of simple actions"
                    , proxy_ty: Proxy :: Proxy (Array String)
                    }
              }
          }
      }
    sequentialize_tools = C.toTools_fromActionsSpec sequentialize_actionsSpec

  let tools = C.toTools_fromActionsSpec model.actionsSpec

  state_ref <- model.initialState >>= Ref.new >>> liftEffect
  client <- Ai.make_client { token: Secret.cohere_token } # liftEffect

  let
    handleAction :: Variant actions -> Aff Unit
    handleAction action = do
      state <- state_ref # Ref.read # liftEffect
      flip Ref.write state_ref >>> liftEffect =<< model.updateState action state

    execPrompt :: String -> Aff Unit
    execPrompt prompt = do
      preactions :: Array String <- do
        state <- state_ref # Ref.read # liftEffect
        response <- Ai.chat client
          { model: "command-r-plus"
          , preamble:
              """
## Context

""" <> model.describeContext state
                <>
                  """

## Task

The user will give you instructions in natural language.
Your task is to interpret these instructions into a sequence of simple actions.

Each step should be only a single simple action, where the available actions are:
"""
                <>
                  ( tools
                      # map (\{ name, description } -> "  - " <> name <> ": " <> description)
                      # Array.intercalate "\n"
                  )
                <>
                  """

Note that the user's instructions can be vague, so you must do your best to interpret them into the MOST REASONABLE form of the simple actions described above.

Make sure to use the `submit_interpretation` tool to submit your interpretation the sequence of actions.
"""
          , chatHistory: []
          , message:
              """
The current state is:

"""
                <> (model.describeState state # indent 4)
                <>
                  """

Submit your interpretation of the user's instructions as a sequence of simple actions.
The user's instructions are:

"""
                <> (prompt # indent 4)
                <> "\n"
          , forceSingleStep: true
          , tools: sequentialize_tools
          }
        response.toolCalls
          # traverse
              ( \toolCall -> do
                  action <-
                    C.fromToolCall_toAction sequentialize_actionsSpec toolCall
                      # runExceptT
                      # bindFlipped case _ of
                          Left err -> throwError (error err)
                          Right action -> pure action
                  action
                    # V.match
                        { submit_interpretation: \(C.Action { actions }) -> pure actions
                        }
              )
          # map Array.fold

      preactions
        # traverse_
            ( \preaction -> do
                state <- state_ref # Ref.read # liftEffect
                response <- Ai.chat client
                  { model: "command-r-plus"
                  , preamble:
                      """
## Context

""" <> model.describeContext state
                        <>
                          """

## Current State

"""
                        <> (model.describeState state # indent 4)
                        <>
                          """

## Task

The user will give you an instruction in natural language.
Your task is to interpret this informal instruction by using the corresponding tool to accomplish what the user intended.
"""
                  , chatHistory: []
                  , message: preaction
                  , forceSingleStep: true
                  , tools
                  }
                response.toolCalls # traverse_
                  ( \toolCall -> do
                      action <- C.fromToolCall_toAction model.actionsSpec toolCall
                        # runExceptT
                        # bindFlipped (either (\err -> throwError (error err)) pure)
                      handleAction action
                  )
            )

  let
    go :: forall a. Unit -> Aff a
    go it = do
      prompt <- model.getPrompt =<< (state_ref # Ref.read # liftEffect)
      execPrompt prompt
      go it

  go unit

indent :: Int -> String -> String
indent n =
  String.split (String.Pattern "\n")
    >>> map ((space # Array.replicate n # String.fromCodePointArray) <> _)
    >>> String.joinWith "\n"
  where
  space = ' ' # CodePoints.codePointFromChar
