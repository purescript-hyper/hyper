module Hyper.Request
  ( class Request
  , RequestData
  , getRequestData
  , class BaseRequest
  , class ReadableBody
  , readBody
  , class StreamableBody
  , streamBody
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

-- | A `ReadableBody` instance reads the complete request body as a
-- | value of type `b`. For streaming the request body, see the
-- | [StreamableBody](#streamablebody) class.
class ReadableBody req m b where
  readBody
    :: forall res c
     . Middleware
       m
       (Conn req res c)
       (Conn req res c)
       b

-- | A `StreamableBody` instance returns a stream of the request body,
-- | of type `stream`. To read the whole body as a value, without
-- | streaming, see the [ReadableBody](#readablebody) class.
class StreamableBody req m stream | req -> stream where
  streamBody
    :: forall res c
     . Middleware
       m
       (Conn req res c)
       (Conn req res c)
       stream
