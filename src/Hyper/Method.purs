module Hyper.Method where

import Data.Eq (class Eq)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Show (class Show)

data Method = GET | POST

derive instance eqMethod :: Eq Method

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"

fromString :: String -> Maybe Method
fromString s =
  case s of
    "GET" -> Just GET
    "POST" -> Just POST
    _ -> Nothing
