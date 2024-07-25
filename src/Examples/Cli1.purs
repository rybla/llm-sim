module Examples.Cli1 where

import Prelude

import App.Aff as App
import Common as C
import Data.Options ((:=))
import Data.String as String
import Data.Variant as V
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Node.Process as Process
import Node.ReadLine as ReadLine
import Node.ReadLine.Aff as ReadLine.Aff
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = App.runModel { getPrompt } model # Aff.launchAff_
  where

  getPrompt state@{ interface } =
    ReadLine.Aff.question
      (describeState state <> "\n\nprompt: ")
      interface

  describeContext _ = "You are controlling a robot with one dimension of movement and a colored light attachment."

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

  describeState { position, color } =
    [ ""
    , "position: " <> show position
    , "color: " <> show color
    ] # String.joinWith "\n"

  model = C.Model
    { actionsSpec

    , initialState: do
        interface <-
          ReadLine.createInterface
            Process.stdin
            (ReadLine.output := Process.stdout)
            # liftEffect
        pure
          { position: 0
          , color: "white"
          , interface
          }

    , updateState: V.match
        { move: \(C.Action { distance }) state@{ position } -> pure state { position = position + distance }
        , set_light_color: \(C.Action { color }) state -> pure state { color = color }
        }

    , describeState
    , describeContext

    }

