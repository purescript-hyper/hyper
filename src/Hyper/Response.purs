module Hyper.Response where

import Prelude
import Control.IxMonad ((:>>=), (:*>))
import Data.Foldable (traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable)
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


-- | A middleware transitioning from one `ResponseWriter` state to another.
type ResponseStateTransition m rw from to =
  forall req res c.
  Middleware
  m
  (Conn req {writer :: rw from | res} c)
  (Conn req {writer :: rw to | res} c)
  Unit

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class ResponseWriter rw m b | rw -> b where
  writeStatus :: Status -> ResponseStateTransition m rw StatusLineOpen HeadersOpen
  writeHeader :: Header -> ResponseStateTransition m rw HeadersOpen HeadersOpen
  closeHeaders :: ResponseStateTransition m rw HeadersOpen BodyOpen
  send :: b -> ResponseStateTransition m rw BodyOpen BodyOpen
  end :: ResponseStateTransition m rw BodyOpen ResponseEnded

headers :: forall t m req res rw b c.
           (Traversable t, Monad m, ResponseWriter rw m b) =>
           t Header
        -> Middleware
           m
           (Conn req { writer :: rw HeadersOpen | res } c)
           (Conn req { writer :: rw BodyOpen | res } c)
           Unit
headers hs =
  traverse_ writeHeader hs
  :*> closeHeaders

contentType :: forall m req res rw b c.
               (Monad m, ResponseWriter rw m b) =>
               MediaType
            -> Middleware
               m
               (Conn req { writer :: rw HeadersOpen | res } c)
               (Conn req { writer :: rw HeadersOpen | res } c)
               Unit
contentType mediaType = writeHeader (Tuple "Content-Type" (unwrap mediaType))

redirect
  :: forall m req res rw b c
   . ( Monad m
     , ResponseWriter rw m b
     )
  => String
  -> Middleware
      m
      (Conn req { writer :: rw StatusLineOpen | res } c)
      (Conn req { writer :: rw HeadersOpen | res } c)
      Unit
redirect uri =
  writeStatus statusFound
  :*> writeHeader (Tuple "Location" uri)

class Response b m r where
  toResponse :: forall i. r -> Middleware m i i b

respond :: forall m r b req res rw c.
           ( Monad m
           , Response b m r
           , ResponseWriter rw m b
           ) =>
           r
        -> Middleware
           m
           (Conn req { writer :: rw BodyOpen | res } c)
           (Conn req { writer :: rw ResponseEnded | res } c)
           Unit
respond r = (toResponse r :>>= send) :*> end
