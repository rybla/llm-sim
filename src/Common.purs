module Common where

import AI
import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (either)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- =============================================================================
-- Model

newtype Model state actionsSpec actions m = Model
  { actionsSpec :: Record actionsSpec
  , initialState :: m state
  , updateState :: Variant actions -> state -> m state
  , describeState :: state -> String
  , describeContext :: state -> String
  }

-- =============================================================================
-- ActionsSpec

class ActionsSpec actionsSpec actions | actionsSpec -> actions where
  toTools_fromActionsSpec :: Record actionsSpec -> Tools
  fromToolCall_toAction :: forall m. MonadError String m => Record actionsSpec -> ToolCall -> m (Variant actions)

instance
  ( RowToList actionsSpec actionsSpecList
  , ActionsSpec_map actionsSpec actionsSpecList actions
  ) =>
  ActionsSpec actionsSpec actions where
  toTools_fromActionsSpec = toTools_fromActionsSpec_map (Proxy :: Proxy actionsSpecList)
  fromToolCall_toAction = fromToolCall_toAction_map (Proxy :: Proxy actionsSpecList)

class ActionsSpec_map actionsSpec (actionsSpecList :: RowList Type) actions | actionsSpec actionsSpecList -> actions where
  toTools_fromActionsSpec_map :: Proxy actionsSpecList -> Record actionsSpec -> Tools
  fromToolCall_toAction_map :: forall m. MonadError String m => Proxy actionsSpecList -> Record actionsSpec -> ToolCall -> m (Variant actions)

instance ActionsSpec_map actionsSpec RL.Nil actions where
  toTools_fromActionsSpec_map _ _ = []
  fromToolCall_toAction_map _ _actionsSpec { name } = throwError name

instance
  ( IsSymbol name
  , ActionParamsSpec actionParamsSpec actionParams
  , Cons name (ActionSpec actionParamsSpec) actionsSpec_ actionsSpec
  , Cons name (Action actionParams) actions_ actions
  , ActionsSpec_map actionsSpec actionsSpecList actions
  ) =>
  ActionsSpec_map actionsSpec (RL.Cons name (ActionSpec actionParamsSpec) actionsSpecList) actions
  where
  toTools_fromActionsSpec_map _ actionsSpec =
    let
      ActionSpec actionSpec = actionsSpec # Record.get (Proxy :: Proxy name)
    in
      { name: reflectSymbol (Proxy :: Proxy name)
      , description: actionSpec.description
      , parameterDefinitions: toToolParameterDefinitionsValues_fromActionParamsSpec actionSpec.params
      } Array.:
        toTools_fromActionsSpec_map (Proxy :: Proxy actionsSpecList) actionsSpec
  fromToolCall_toAction_map _ actionsSpec toolCall@{ name, parameters } =
    if name == reflectSymbol (Proxy :: Proxy name) then
      let
        ActionSpec actionSpec = actionsSpec # Record.get (Proxy :: Proxy name)
      in
        V.inj (Proxy :: Proxy name) <$> (Action <$> fromToolCallParameters_toActionParams actionSpec.params parameters)
    else
      fromToolCall_toAction_map (Proxy :: Proxy actionsSpecList) actionsSpec toolCall

newtype ActionSpec actionParamsSpec = ActionSpec { description :: String, params :: Record actionParamsSpec }

newtype Action actionParams = Action (Record actionParams)

-- =============================================================================
-- ActionParamsSpec

class ActionParamsSpec actionParamsSpec actionParams | actionParamsSpec -> actionParams where
  toToolParameterDefinitionsValues_fromActionParamsSpec :: Record actionParamsSpec -> ToolParameterDefinitionsValues
  fromToolCallParameters_toActionParams :: forall m. MonadError String m => Record actionParamsSpec -> ToolCallParameters -> m (Record actionParams)

instance
  ( RowToList actionParamsSpec actionParamsSpecList
  , ActionParamsSpec_map actionParamsSpec actionParamsSpecList actionParams
  , DecodeJson (Record actionParams)
  ) =>
  ActionParamsSpec actionParamsSpec actionParams where
  toToolParameterDefinitionsValues_fromActionParamsSpec = toToolParameterDefinitionsValues_fromActionParamsSpec_map (Proxy :: Proxy actionParamsSpecList)
  fromToolCallParameters_toActionParams _actionParamsSpec = decodeJson >>> either (throwError <<< printJsonDecodeError) pure

class
  ActionParamsSpec_map actionParamsSpec (actionParamsSpecList :: RowList Type) (actionParams :: Row Type)
  | actionParamsSpecList -> actionParamsSpec actionParams where
  toToolParameterDefinitionsValues_fromActionParamsSpec_map :: Proxy actionParamsSpecList -> Record actionParamsSpec -> ToolParameterDefinitionsValues

instance ActionParamsSpec_map () RL.Nil () where
  toToolParameterDefinitionsValues_fromActionParamsSpec_map _ _actionParamsSpec = Object.empty

instance
  ( IsSymbol name
  , ActionParamType ty
  , Lacks name actionParamsSpec_
  , Cons name (ActionParamSpec ty) actionParamsSpec_ actionParamsSpec
  , Lacks name actionParams_
  , Cons name ty actionParams_ actionParams
  , ActionParamsSpec_map actionParamsSpec_ actionParamsSpecList actionParams_
  ) =>
  ActionParamsSpec_map actionParamsSpec (RL.Cons name (ActionParamSpec ty) actionParamsSpecList) actionParams
  where
  toToolParameterDefinitionsValues_fromActionParamsSpec_map _ actionParamsSpec =
    let
      ActionParamSpec actionParamSpec = actionParamsSpec # Record.get (Proxy :: Proxy name)
    in
      Object.insert (reflectSymbol (Proxy :: Proxy name))
        { description: actionParamSpec.description
        , required: true
        , type: actionParamSpec.proxy_ty # toToolParamType
        } $
        toToolParameterDefinitionsValues_fromActionParamsSpec_map (Proxy :: Proxy actionParamsSpecList) (actionParamsSpec # Record.delete (Proxy :: Proxy name))

newtype ActionParamSpec (ty :: Type) = ActionParamSpec
  { description :: String
  , proxy_ty :: Proxy ty
  -- TODO: figure out how to handle optional arguments in Json decoding
  }

-- =============================================================================
-- ActionParamType

class ActionParamType (a :: Type) where
  toToolParamType :: Proxy a -> String

instance ActionParamType String where
  toToolParamType _ = "str"

instance ActionParamType Int where
  toToolParamType _ = "int"

instance ActionParamType a => ActionParamType (Array a) where
  toToolParamType _ = "list[" <> toToolParamType (Proxy :: Proxy a) <> "]"

-- =============================================================================
-- Example

myActionsSpec =
  { move: ActionSpec
      { description: "Move a distance"
      , params:
          { distance: ActionParamSpec
              { description: "The distance to move by"
              , proxy_ty: Proxy :: Proxy Int
              }
          }
      }
  }

xxx = toTools_fromActionsSpec myActionsSpec
