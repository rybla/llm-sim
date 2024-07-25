module App.Halogen where

import Prelude

import AI as Ai
import Common as C
import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.String as String
import Data.String.CodePoints as CodePoints
import Data.Traversable (traverse, traverse_)
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Halogen as H
import Halogen.Aff as Halogen.Aff
import Halogen.Aff as Halogen.Aff.Util
import Halogen.HTML as HH
import Halogen.VDom.Driver as Halogen.VDom.Driver
import Secret as Secret
import Type.Proxy (Proxy(..))

runApp :: forall state actionsSpec actions. C.ActionsSpec actionsSpec actions => AppInput state actionsSpec actions -> Effect Unit
runApp input = Halogen.Aff.runHalogenAff (Halogen.VDom.Driver.runUI appComponent input =<< Halogen.Aff.Util.awaitBody)

-- =============================================================================

type AppQuery state actionsSpec actions = Identity

type AppInput state actionsSpec actions =
  { model :: C.Model state actionsSpec actions Aff
  , promptComponent :: PromptComponent
  , viewComponent :: ViewComponent state
  }

type AppOutput state actionsSpec actions = Void

newtype AppState state actionsSpec actions = AppState
  { model :: C.Model state actionsSpec actions Aff
  , tools :: Ai.Tools
  , mb_initialState :: Maybe state
  , mb_client :: Maybe Ai.Client
  , promptComponent :: PromptComponent
  , viewComponent :: ViewComponent state
  }

derive instance Newtype (AppState state actionsSpec actions) _

data AppAction state actionsSpec actions
  = Initialize_AppAction
  | SubmitPrompt_AppAction String

type AppSlots state actionsSpec actions =
  ( prompt :: H.Slot PromptQuery PromptOutput Unit
  , view :: H.Slot (ViewQuery state) (ViewOutput state) Unit
  )

type AppM state actionsSpec actions = H.HalogenM (AppState state actionsSpec actions) (AppAction state actionsSpec actions) (AppSlots state actionsSpec actions) (AppOutput state actionsSpec actions) Aff

appComponent
  :: forall state actionsSpec actions
   . C.ActionsSpec actionsSpec actions
  => H.Component (AppQuery state actionsSpec actions) (AppInput state actionsSpec actions) (AppOutput state actionsSpec actions) Aff
appComponent = H.mkComponent { initialState: initialComponentState, eval, render }
  where

  initialComponentState :: AppInput state actionsSpec actions -> AppState state actionsSpec actions
  initialComponentState { model: model@(C.Model { actionsSpec }), promptComponent, viewComponent } =
    AppState
      { model
      , tools: C.toTools_fromActionsSpec actionsSpec
      , mb_initialState: Nothing
      , mb_client: Nothing
      , promptComponent
      , viewComponent
      }

  eval = H.mkEval H.defaultEval { initialize = Just Initialize_AppAction, handleAction = handleAction }

  sequentialize_actionsSpec =
    { submit_interpretation: C.ActionSpec
        { description: "Submit a sequence of simple actions as a structured interpretation of the user prompt."
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

  getModelViewState :: _ state
  getModelViewState = H.request (Proxy :: Proxy "view") unit GetState_ViewQuery >>= maybe (throwError (Aff.error "[getModelViewState] view component does not exist")) pure

  getClient :: _ Ai.Client
  getClient = H.gets (unwrap >>> _.mb_client) >>= maybe (throwError (Aff.error "[getClient] client has not yet been initialized")) pure

  handleAction = case _ of

    Initialize_AppAction -> do
      AppState { model: C.Model model } <- H.get
      initialState <- model.initialState # H.liftAff
      client <- Ai.make_client { token: Secret.cohere_token } # H.liftEffect
      H.modify_ (over AppState _ { mb_initialState = Just initialState, mb_client = Just client })
      pure unit

    SubmitPrompt_AppAction prompt -> do
      H.tell (Proxy :: Proxy "view") unit (SetAnnotation_ViewQuery prompt)
      client <- getClient
      AppState { model: model@(C.Model { actionsSpec, updateState }), tools } <- H.get
      preactions :: Array String <- do
        modelState <- getModelViewState
        sequentializePromptToPreactions_chat { client, model, tools, state: modelState, prompt, sequentialize_tools }
          # H.liftAff
          # map _.toolCalls
          # bindFlipped
              ( traverse \toolCall ->
                  toolCall
                    # C.fromToolCall_toAction sequentialize_actionsSpec
                    # runExceptT
                    # bindFlipped (either (\err -> throwError (Aff.error err)) pure)
                    # bindFlipped (V.match { submit_interpretation: \(C.Action { actions }) -> pure actions })
              )
          # map Array.fold
      preactions
        # traverse_
            ( \preaction -> do
                modelState <- getModelViewState
                interpretPreactionToAction_chat { client, model, preaction, state: modelState, tools }
                  # H.liftAff
                  # map _.toolCalls
                  # bindFlipped
                      ( traverse
                          ( \toolCall ->
                              C.fromToolCall_toAction actionsSpec toolCall
                                # runExceptT
                                # bindFlipped (either (\err -> throwError (Aff.error err)) pure)
                                # bindFlipped (\action -> H.tell (Proxy :: Proxy "view") unit (UpdateState_ViewQuery prompt (updateState action)))
                          )
                      )
            )
      pure unit

  render (AppState { mb_initialState, promptComponent, viewComponent }) =
    HH.div
      []
      ( [ [ HH.slot (Proxy :: Proxy "prompt") unit promptComponent {}
              case _ of
                SubmitPrompt_PromptOutput prompt -> SubmitPrompt_AppAction prompt
          ]
        , case mb_initialState of
            Nothing -> []
            Just state -> [ HH.slot_ (Proxy :: Proxy "view") unit viewComponent { state } ]
        ] # Array.fold
      )

-- =============================================================================

type PromptComponent = H.Component PromptQuery PromptInput PromptOutput Aff

type PromptQuery = Identity

type PromptInput = {}

data PromptOutput = SubmitPrompt_PromptOutput String

-- =============================================================================

type ViewComponent state = H.Component (ViewQuery state) (ViewInput state) (ViewOutput state) Aff

data ViewQuery state (a :: Type)
  = GetState_ViewQuery (state -> a)
  | UpdateState_ViewQuery String (state -> Aff state) a
  | SetAnnotation_ViewQuery String a

type ViewInput state =
  { state :: state }

type ViewOutput (state :: Type) = Void

type ViewState state =
  { state :: state
  }

-- =============================================================================

sequentializePromptToPreactions_chat { client, model: C.Model model, tools, state, prompt, sequentialize_tools } = Ai.chat client
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

interpretPreactionToAction_chat { client, model: C.Model model, state, preaction, tools } = Ai.chat client
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

indent :: Int -> String -> String
indent n =
  String.split (String.Pattern "\n")
    >>> map ((space # Array.replicate n # String.fromCodePointArray) <> _)
    >>> String.joinWith "\n"
  where
  space = ' ' # CodePoints.codePointFromChar
