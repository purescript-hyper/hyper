module Hyper.Response where

import Prelude
import Control.IxMonad (ibind)
import Data.Foldable (traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn)
import Hyper.Core (class ResponseWriter, BodyOpen, HeadersOpen, ResponseEnded, Header, closeHeaders, end, send, writeHeader)
import Hyper.Middleware (Middleware)

headers :: forall t m req res rw b c.
           (Traversable t, Monad m, ResponseWriter rw m b) =>
           t Header
        -> Middleware
           m
           (Conn req { writer :: rw HeadersOpen | res } c)
           (Conn req { writer :: rw BodyOpen | res } c)
           Unit
headers hs = do
  traverse_ writeHeader hs
  closeHeaders
  where
    bind = ibind

contentType :: forall m req res rw b c.
               (Monad m, ResponseWriter rw m b) =>
               MediaType
            -> Middleware
               m
               (Conn req { writer :: rw HeadersOpen | res } c)
               (Conn req { writer :: rw HeadersOpen | res } c)
               Unit
contentType mediaType = writeHeader (Tuple "Content-Type" (unwrap mediaType))

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
respond r = do
  body <- toResponse r
  send body
  end
  where bind = ibind
