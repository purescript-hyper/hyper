module Hyper.Request
  ( class Request
  , RequestData
  , getRequestData
  , class BaseRequest
  , class ReadableBody
  , readBody
  ) where

import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)

type RequestData =
  { url :: String
  , contentLength :: Maybe Int
  , headers :: StrMap String
  , method :: Either Method CustomMethod
  }

class Request req m where
  getRequestData
    :: forall res c
     . Middleware
       m
       (Conn req res c)
       (Conn req res c)
       RequestData

class Request req m <= BaseRequest req m

-- | A ReadableBody instance reads the request body for a specific body
-- | reader type.
class ReadableBody req m b | req -> b where
  readBody
    :: forall res c.
       Middleware
       m
       (Conn req res c)
       (Conn req res c)
       b
