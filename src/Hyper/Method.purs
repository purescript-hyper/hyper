module Hyper.Method where

import Data.Maybe (Maybe(Nothing, Just))
import Data.Show (class Show)

data Method = GET | POST

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"

fromString :: String -> Maybe Method
fromString s =
  case s of
    "GET" -> Just GET
    "POST" -> Just POST
    _ -> Nothing
