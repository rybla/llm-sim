module AI where

import Effect (Effect)

foreign import data Client :: Type

foreign import make_client :: { token :: String } -> Effect Client