module Examples.Hyper1 where

import Prelude

import App.Halogen (PromptOutput(..), ViewQuery(..), runApp)
import Common as C
import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Variant as V
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.KeyboardEvent as KeyboardEvent

main :: Effect Unit
main = runApp { model, promptComponent, viewComponent }
  where

  model = C.Model
    let
      actionsSpec =
        { move: C.ActionSpec
            { description: "Move a distance"
            , params:
                { distance: C.ActionParamSpec
                    { description: "The distance to move by"
                    , proxy_ty: Proxy :: Proxy Int
                    }
                }
            }
        , set_light_color: C.ActionSpec
            { description: "Set the robot's light color."
            , params:
                { color: C.ActionParamSpec
                    { description: "A string representing the robot's new light color"
                    , proxy_ty: Proxy :: Proxy String
                    }
                }
            }
        }

    in
      { actionsSpec

      , initialState: do
          pure
            { position: 0
            , color: "white"
            }

      , updateState: V.match
          { move: \(C.Action { distance }) state@{ position } -> pure state { position = position + distance }
          , set_light_color: \(C.Action { color }) state -> pure state { color = color }
          }

      , describeContext: \_ ->
          "You are controlling a robot with one dimension of movement and a colored light attachment."

      , describeState: \{ position, color } ->
          [ ""
          , "position: " <> show position
          , "color: " <> show color
          ] # String.joinWith "\n"

      }

  promptComponent = H.mkComponent { initialState, eval, render }
    where
    initialState {} = { prompt: "" }

    -- _SubmitPrompt = Proxy :: Proxy "SubmitPrompt"
    _OnKeyDown = Proxy :: Proxy "OnKeyDown"
    -- _OnPromptChange = Proxy :: Proxy "OnPromptChange"

    eval = H.mkEval H.defaultEval { handleAction = handleAction }

    handleAction = V.match
      { "OnPromptChange": \event -> do
          prompt <- event
            # Event.target
            # maybe (throwError (Exception.error "Event.target == Nothing")) pure
            # bindFlipped (HTMLInputElement.fromEventTarget >>> pure)
            # bindFlipped (maybe (throwError (Exception.error "HTMLInputElement.fromEventTarget ... = Nothing")) pure)
            # bindFlipped (HTMLInputElement.value >>> H.liftEffect)
          H.modify_ _ { prompt = prompt }
      , "OnKeyDown": \event -> do
          prompt <- event
            # KeyboardEvent.toEvent
            # Event.target
            # maybe (throwError (Exception.error "Event.target == Nothing")) pure
            # bindFlipped (HTMLInputElement.fromEventTarget >>> pure)
            # bindFlipped (maybe (throwError (Exception.error "HTMLInputElement.fromEventTarget ... = Nothing")) pure)
            # bindFlipped (HTMLInputElement.value >>> H.liftEffect)
          H.modify_ _ { prompt = prompt }
          
          when (KeyboardEvent.key event == "Enter") do
            { prompt } <- H.get
            H.modify_ _ { prompt = "" }
            H.raise (SubmitPrompt_PromptOutput prompt)
      }

    render { prompt } =
      HH.div
        [ HP.style "display: flex; flex-direction: column; padding: 0.5em; margin: 0.5em; box-shadow: 0 0 0 0.1em black inset;" ]
        [ HH.input
            [ HE.onKeyDown (V.inj _OnKeyDown)
            , HP.value prompt
            ]
        ]

  viewComponent = H.mkComponent { initialState, eval, render }
    where
    initialState { state } =
      { state
      , mb_prompt: Nothing :: Maybe String
      }

    eval = H.mkEval H.defaultEval { handleQuery = handleQuery }

    handleQuery :: forall a. ViewQuery _ a -> _ (Maybe a)
    handleQuery = case _ of
      GetState_ViewQuery k -> do
        { state } <- H.get
        pure (Just (k state))
      UpdateState_ViewQuery prompt f a -> do
        st@{ state } <- H.get
        -- H.put =<< (f state # H.liftAff # map (st { mb_prompt = Nothing, state = _ }))
        H.put =<< (f state # H.liftAff # map (st { state = _ }))
        pure (Just a)
      SetAnnotation_ViewQuery prompt a -> do
        H.modify_ _ { mb_prompt = Just prompt }
        pure (Just a)

    render { state: { position, color }, mb_prompt } =
      HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 1em;" ]
        ( [ [ HH.div
                [ HP.style "margin: 0.5em; padding: 0.5em; display: flex; flex-direction: row; gap: 0.5em; box-shadow: 0 0 0 0.1em black inset; width: 80vw; overflow-x: scroll;" ]
                ( Array.range 0 20
                    # map
                        ( \i ->
                            HH.div
                              [ HP.style
                                  ( "display: flex; flex-direction: column; gap: 0.5em; padding: 0.5em; min-width: 1.5em; box-shadow: 0 0 0 0.2em black inset; " <>
                                      if i /= position then ""
                                      else
                                        "background-color: " <> color <> "; "
                                  )
                              ]
                              ( [ HH.div [] [ HH.text (show i) ] ] <>
                                  if i /= position then []
                                  else
                                    [ HH.div [] [ HH.text "X" ] ]
                              )
                        )
                )
            ]
          , case mb_prompt of
              Nothing -> []
              Just prompt -> [ HH.div [ HP.style "padding: 0.5em; font-style: italic" ] [ HH.text prompt ] ]
          ] # Array.fold
        )

