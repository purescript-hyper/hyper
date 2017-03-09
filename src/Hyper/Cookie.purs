module Hyper.Cookie
       ( Name
       , Values
       ) where

import Data.NonEmpty (NonEmpty)

type Name = String
type Values = NonEmpty Array String
