module Hyper.Response where

import Prelude

import Control.Monad.Indexed ((:>>=), (:*>))
import Data.Foldable (class Foldable, traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (BodyOpen, HeadersOpen, NoTransition, ResponseEnded, ResponseTransition, StatusLineOpen, kind ResponseState)
import Hyper.Header (Header)
import Hyper.Middleware (Middleware)
import Hyper.Status (Status, statusFound)

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class Response (res :: ResponseState -> Type) m b | res -> b where
  writeStatus
    :: forall req reqState comp
     . Status
    -> ResponseTransition m req reqState res StatusLineOpen HeadersOpen comp Unit
  writeHeader
    :: forall req reqState comp
     . Header
    -> NoTransition m req reqState res HeadersOpen comp Unit
  closeHeaders
    :: forall req reqState comp
     . ResponseTransition m req reqState res HeadersOpen BodyOpen comp Unit
  send
    :: forall req reqState comp
     . b
    -> NoTransition m req reqState res BodyOpen comp Unit
  end
    :: forall req reqState comp
     . ResponseTransition m req reqState res BodyOpen ResponseEnded comp Unit

headers
  :: forall f m req reqState (res :: ResponseState -> Type) b comp
  .  Foldable f
  => Monad m
  => Response res m b
  => f Header
  -> ResponseTransition m req reqState res HeadersOpen BodyOpen comp Unit
headers hs =
  traverse_ writeHeader hs
  :*> closeHeaders

contentType
  :: forall m req reqState (res :: ResponseState -> Type) b comp
  .  Monad m
  => Response res m b
   => MediaType
   -> NoTransition m req reqState res HeadersOpen comp Unit
contentType mediaType =
  writeHeader (Tuple "Content-Type" (unwrap mediaType))

redirect
  :: forall m req reqState (res :: ResponseState -> Type) b comp
  .  Monad m
  => Response res m b
  => String
  -> ResponseTransition m req reqState res StatusLineOpen HeadersOpen comp Unit
redirect uri =
  writeStatus statusFound
  :*> writeHeader (Tuple "Location" uri)

class ResponseWritable b m r where
  toResponse :: forall i. r -> Middleware m i i b

respond
  :: forall m r b req reqState (res :: ResponseState -> Type) comp
  .  Monad m
  => ResponseWritable b m r
  => Response res m b
  => r
  -> ResponseTransition m req reqState res BodyOpen ResponseEnded comp Unit
respond r = (toResponse r :>>= send) :*> end
