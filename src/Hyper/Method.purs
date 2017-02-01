module Hyper.Method where

import Data.Eq (class Eq)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Show (class Show)

data Method
  = OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | PATCH
  | DELETE
  | TRACE
  | CONNECT

derive instance eqMethod :: Eq Method

instance showMethod :: Show Method where
  show OPTIONS = "OPTIONS"
  show GET = "GET"
  show HEAD = "HEAD"
  show POST = "POST"
  show PUT = "PUT"
  show PATCH = "PATCH"
  show DELETE = "DELETE"
  show TRACE = "TRACE"
  show CONNECT = "CONNECT"

fromString :: String -> Maybe Method
fromString s =
  case s of
    "OPTIONS" -> Just OPTIONS
    "GET" -> Just GET
    "HEAD" -> Just HEAD
    "POST" -> Just POST
    "PUT" -> Just PUT
    "PATCH" -> Just PATCH
    "DELETE" -> Just DELETE
    "TRACE" -> Just TRACE
    "CONNECT" -> Just CONNECT
    _ -> Nothing
