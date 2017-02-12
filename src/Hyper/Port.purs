module Hyper.Port where

import Data.Newtype (class Newtype)

newtype Port = Port Int

derive instance newtypePort :: Newtype Port _
