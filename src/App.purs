module App where

import Common
import Prelude

import AI as Ai
import Common as C
import Control.Bind (bindFlipped)
import Control.Monad.Except (runExceptT, throwError)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Variant (Variant)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith)
import Secret as Secret

runModel
  :: forall state actionsSpec actions
   . ActionsSpec actionsSpec actions
  => Model state actionsSpec actions
  -> Aff Unit
runModel (Model model) = do
  let tools = C.toTools_fromActionSpec model.actionsSpec
  state_ref <- model.initialState >>= Ref.new >>> liftEffect
  client <- Ai.make_client { token: Secret.cohere_token } # liftEffect
  let
    handleAction :: Variant actions -> Aff Unit
    handleAction action = do
      state <- state_ref # Ref.read # liftEffect
      flip Ref.write state_ref >>> liftEffect =<< model.updateState action state

    queryAi :: String -> Aff (Array (Variant actions))
    queryAi prompt = do
      state <- state_ref # Ref.read # liftEffect
      preactions <- do 
        response <- client `Ai.chat` { model: "command-r-plus"
        , preamble: "TODO: preamble"
        , chatHistory: [] -- TODO
        , message: "TODO: build message from prompt"
        , forceSingleStep: false
        , tools: []
        }
        pure []

      response <- client `Ai.chat`
        { model: "command-r-plus"
        , preamble: "TODO: preamble"
        , chatHistory: [] -- TODO
        , message: "TODO: build message from prompt"
        , forceSingleStep: true
        , tools
        }
      response.toolCalls # traverse_ \toolCall -> do
        action <-
          fromToolCall_toAction model.actionsSpec toolCall
            # runExceptT
            # bindFlipped case _ of
                Left err -> throwError (error err)
                Right action -> pure action
        pure unit
      pure [] -- TODO

    go :: forall a. Unit -> Aff a
    go it = do
      prompt <- model.getPrompt =<< (state_ref # Ref.read # liftEffect)
      actions <- queryAi prompt
      actions # traverse_ handleAction
      go it
  go unit

-- type AppQuery state actionsSpec actions = Identity

-- type AppInput state actionsSpec actions =
--   { model :: Model state actionsSpec actions
--   }

-- type AppOutput state actionsSpec actions = Void

-- type AppState state actionsSpec actions =
--   { model :: Model state actionsSpec actions
--   , mb_state :: Maybe state
--   }

-- data AppAction state actionsSpec actions =
--   Initialize_AppAction

-- app_component :: forall state actionsSpec actions. H.Component (AppQuery state actionsSpec actions) (AppInput state actionsSpec actions) (AppOutput state actionsSpec actions) Aff
-- app_component = H.mkComponent { initialState, eval, render }
--   where
--   initialState :: AppInput state actionsSpec actions -> AppState state actionsSpec actions
--   initialState { model } = { model, mb_state: Nothing }

--   eval = H.mkEval H.defaultEval { initialize = Just Initialize_AppAction, handleAction = handleAction }

--   handleAction = case _ of
--     Initialize_AppAction -> do
--       { model: Model model } <- H.get
--       state <- model.initialState # H.liftAff
--       H.modify_ _ { mb_state = Just state }
--       pure unit

--   render { model } = HH.div [] []
