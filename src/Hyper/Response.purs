module Hyper.Response where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed ((:>>=), (:*>))
import Data.Foldable (class Foldable, traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn)
import Hyper.Header (Header)
import Hyper.Middleware (Middleware)
import Hyper.Status (Status, statusFound)

-- | Type indicating that the status-line is ready to be
-- | sent.
data StatusLineOpen

-- | Type indicating that headers are ready to be
-- | sent, i.e. the body streaming has not been started.
data HeadersOpen

-- | Type indicating that headers have already been
-- | sent, and that the body is currently streaming.
data BodyOpen

-- | Type indicating that headers have already been
-- | sent, and that the body stream, and thus the response,
-- | is finished.
data ResponseEnded


-- | A middleware transitioning from one `Response` state to another.
type ResponseStateTransition m res from to =
  forall req c.
  Middleware
  m
  (Conn req (res from) c)
  (Conn req (res to) c)
  Unit

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class Response (res :: Type -> Type) m b | res -> b where
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
  :: forall f m req res b c
  .  Foldable f
  => Monad m
  => Response res m b
  => f Header
  -> Middleware
     m
     (Conn req (res HeadersOpen) c)
     (Conn req (res BodyOpen) c)
     Unit
headers hs = Ix.do
  traverse_ writeHeader hs
  closeHeaders

contentType
  :: forall m req res b c
  .  Monad m
  => Response res m b
   => MediaType
   -> Middleware
       m
       (Conn req (res HeadersOpen) c)
       (Conn req (res HeadersOpen) c)
       Unit
contentType mediaType =
  writeHeader (Tuple "Content-Type" (unwrap mediaType))

redirect
  :: forall m req res b c
  .  Monad m
  => Response res m b
  => String
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res HeadersOpen) c)
     Unit
redirect uri = Ix.do
  writeStatus statusFound
  writeHeader (Tuple "Location" uri)

class ResponseWritable b m r where
  toResponse :: forall i. r -> Middleware m i i b

respond
  :: forall m r b req res c
  .  Monad m
  => ResponseWritable b m r
  => Response res m b
  => r
  -> Middleware
     m
     (Conn req (res BodyOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
respond r = (toResponse r :>>= send) :*> end
