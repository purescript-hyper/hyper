module Hyper.Request
  ( class Request
  , RequestData
  , ParsedUrl
  , parseUrl
  , getRequestData
  , class BaseRequest
  , class ReadableBody
  , readBody
  , class StreamableBody
  , streamBody
  ) where

import Prelude
import Data.Array as Array
import Data.String as String
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe, fromMaybe)
import Foreign.Object (Object)
import Data.Tuple (Tuple)
import Hyper.Conn (Conn)
import Hyper.Form.Urlencoded (parseUrlencoded)
import Hyper.Middleware (Middleware)

type RequestData =
  { url :: String
  , parsedUrl :: Lazy ParsedUrl
  , contentLength :: Maybe Int
  , headers :: Object String
  , method :: Either Method CustomMethod
  }

type ParsedUrl =
  { path :: Array String
  , query :: Either String (Array (Tuple String (Maybe String)))
  }

parseUrl :: String -> ParsedUrl
parseUrl url =
  let
    idx = fromMaybe (String.length url) $ String.indexOf (String.Pattern "?") url
    rawPath = String.take idx url
    rawQuery = String.drop (idx + 1) url
    path = Array.filter (_ /= "") $ String.split (String.Pattern "/") rawPath
    query = lmap (const rawQuery) $ parseUrlencoded rawQuery
  in
    {path, query}

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
