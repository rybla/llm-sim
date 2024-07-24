module App.Halogen where

import Prelude

import Common as C
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

type AppQuery :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type -> Type
type AppQuery state actionsSpec actions = Identity

type AppInput state actionsSpec actions =
  { model :: C.Model state actionsSpec actions
  , prompt_component :: PromptComponent
  }

type AppOutput :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
type AppOutput state actionsSpec actions = Void

type AppState state actionsSpec actions =
  { model :: C.Model state actionsSpec actions
  , mb_state :: Maybe state
  , prompt_component :: PromptComponent
  }

data AppAction :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
data AppAction state actionsSpec actions
  = Initialize_AppAction
  | SubmitPrompt_AppAction String

appComponent :: forall state actionsSpec actions. H.Component (AppQuery state actionsSpec actions) (AppInput state actionsSpec actions) (AppOutput state actionsSpec actions) Aff
appComponent = H.mkComponent { initialState, eval, render }
  where
  initialState :: AppInput state actionsSpec actions -> AppState state actionsSpec actions
  initialState { model, prompt_component } = { model, mb_state: Nothing, prompt_component }

  eval = H.mkEval H.defaultEval { initialize = Just Initialize_AppAction, handleAction = handleAction }

  handleAction = case _ of
    Initialize_AppAction -> do
      { model: C.Model model } <- H.get
      state <- model.initialState # H.liftAff
      H.modify_ _ { mb_state = Just state }
      pure unit
    SubmitPrompt_AppAction prompt ->
      -- TODO
      pure unit

  render {} =
    HH.div
      []
      [ HH.slot (Proxy :: Proxy "prompt") unit promptComponent {}
          case _ of
            SubmitPrompt_PromptOutput prompt -> SubmitPrompt_AppAction prompt
      ]

-- =============================================================================

type PromptComponent = H.Component PromptQuery PromptInput PromptOutput Aff

data PromptQuery (a :: Type)

type PromptInput = {}

data PromptOutput = SubmitPrompt_PromptOutput String

promptComponent :: PromptComponent
promptComponent = H.mkComponent { initialState, eval, render }
  where
  initialState = unsafeCrashWith "TODO"

  eval = unsafeCrashWith "TODO"

  render = unsafeCrashWith "TODO"

