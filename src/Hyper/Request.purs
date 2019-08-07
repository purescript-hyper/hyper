module Hyper.Request where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Hyper.Conn (BodyRead, BodyUnread, Conn, NoTransition, kind RequestState, kind ResponseState)
import Hyper.Form.Urlencoded (parseUrlencoded)
import Hyper.Middleware (Middleware)
import Hyper.Middleware.Class (modifyConn)
import Unsafe.Coerce (unsafeCoerce)

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

-- | Alias for the `Conn`'s `reqState` phantom type transitioning
-- | from the `from` RequestState to the `to` RequestState.
type RequestStateTransition m (req :: RequestState -> Type) (from :: RequestState) (to :: RequestState) (res :: ResponseState -> Type) (resState :: ResponseState) comp a =
  Middleware
    m
    (Conn req from res resState comp)
    (Conn req to   res resState comp)
    a

class Request req m where
  getRequestData
    :: forall (reqState :: RequestState) (res :: ResponseState -> Type) (resState :: ResponseState) comp
     . NoTransition m req reqState res resState comp RequestData

class Request req m <= BaseRequest req m

-- | Indicates that this middleware does not read the body of the request.
-- | Useful for situations like the following:
-- | ```purescript
-- | case _ of
-- |   Case1 ->
-- |      void readBody
-- |      writeStatus statusOK
-- |      -- other response writing here
-- |   Case2 ->
-- |      -- body isn't read here
-- |      writeStatus statusOK
-- |      -- other response writing here
-- | ```
-- | In `Case1`, the `RequestState` will be `BodyRead`. However, in
-- | `Case2`, it will be `BodyUnread`. Since the two computations must
-- | return the same type (i.e. the same phantom type for `RequestState`),
-- | the above code will fail to compile because `BodyRead` (Case1) will not
-- | unify with `BodyUnread` (Case2).
-- |
-- | To get deal with this situation, we use this function to forcefully
-- | change a `BodyUnread` RequestState to `BodyRead`.
ignoreBody :: forall m req res resState comp
            . Monad m
           => RequestStateTransition m req BodyUnread BodyRead res resState comp Unit
ignoreBody =
  {-
    The only thing we're doing here is changing the phantome type,
    `reqState`. However, to fully write that change is very verbose
    and requires knowing what the corresponding request type, `req`, is.
    We're essentially unpacking the data wrapped in a data constructor
    that has the old phantom type and rewrapping that data in new
    data constructor with the new phantom type.

    In other words, we would have to write something like this:
      let bind = ibind in do
        conn <- getConn
        let ((HttpRequest data) :: HttpRequest BodyUnread) = conn.request
        let phantomTypeChange = ((HttpRequest data) :: HttpRequest BodyRead)
        putConn (conn { request = phantomTypeChange})

    The more performant route is using `unsafeCoerce`
    to do the same thing.
  -}
  modifyConn unsafeCoerce

-- | A `ReadableBody` instance reads the complete request body as a
-- | value of type `b`. For streaming the request body, see the
-- | [StreamableBody](#streamablebody) class.
class ReadableBody req m b where
  readBody
    :: forall (res :: ResponseState -> Type) (resState :: ResponseState) comp
     . RequestStateTransition m req BodyUnread BodyRead res resState comp b

-- | A `StreamableBody` instance returns a stream of the request body,
-- | of type `stream`. To read the whole body as a value, without
-- | streaming, see the [ReadableBody](#readablebody) class.
class StreamableBody req m stream | req -> stream where
  streamBody
    :: forall (res :: ResponseState -> Type) (resState :: ResponseState) comp
     . (stream -> m Unit)
     -> RequestStateTransition m req BodyUnread BodyRead res resState comp Unit
