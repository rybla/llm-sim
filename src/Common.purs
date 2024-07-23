module Common where

import Prelude

class Model state action where
  update :: action -> state -> state

