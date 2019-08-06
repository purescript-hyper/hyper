module Hyper.Response where

import Prelude

import Control.Monad.Indexed ((:>>=), (:*>))
import Data.Foldable (class Foldable, traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (kind ResponseState, StatusLineOpen, HeadersOpen, BodyOpen, ResponseEnded, Conn)
import Hyper.Header (Header)
import Hyper.Middleware (Middleware)
import Hyper.Status (Status, statusFound)

-- | A middleware transitioning from one `Response` state to another.
type ResponseStateTransition m (res :: ResponseState -> Type) (from :: ResponseState) (to :: ResponseState) =
  forall req comp.
  Middleware
  m
  (Conn req res comp from)
  (Conn req res comp to)
  Unit

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class Response (res :: ResponseState -> Type) m b | res -> b where
  writeStatus
    :: Status
    -> ResponseStateTransition m res StatusLineOpen HeadersOpen
  writeHeader
    :: Header
    -> ResponseStateTransition m res HeadersOpen HeadersOpen
  closeHeaders
    :: ResponseStateTransition m res HeadersOpen BodyOpen
  send
    :: b
    -> ResponseStateTransition m res BodyOpen BodyOpen
  end
    :: ResponseStateTransition m res BodyOpen ResponseEnded

headers
  :: forall f m req (res :: ResponseState -> Type) b comp
  .  Foldable f
  => Monad m
  => Response res m b
  => f Header
  -> Middleware
     m
     (Conn req res comp HeadersOpen)
     (Conn req res comp BodyOpen)
     Unit
headers hs =
  traverse_ writeHeader hs
  :*> closeHeaders

contentType
  :: forall m req (res :: ResponseState -> Type) b comp
  .  Monad m
  => Response res m b
   => MediaType
   -> Middleware
       m
       (Conn req res comp HeadersOpen)
       (Conn req res comp HeadersOpen)
       Unit
contentType mediaType =
  writeHeader (Tuple "Content-Type" (unwrap mediaType))

redirect
  :: forall m req (res :: ResponseState -> Type) b comp
  .  Monad m
  => Response res m b
  => String
  -> Middleware
     m
     (Conn req res comp StatusLineOpen)
     (Conn req res comp HeadersOpen)
     Unit
redirect uri =
  writeStatus statusFound
  :*> writeHeader (Tuple "Location" uri)

class ResponseWritable b m r where
  toResponse :: forall i. r -> Middleware m i i b

respond
  :: forall m r b req (res :: ResponseState -> Type) comp
  .  Monad m
  => ResponseWritable b m r
  => Response res m b
  => r
  -> Middleware
     m
     (Conn req res comp BodyOpen)
     (Conn req res comp ResponseEnded)
     Unit
respond r = (toResponse r :>>= send) :*> end
