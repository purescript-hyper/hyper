module Hyper.Response where

import Prelude
import Data.Foldable (traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (class ResponseWriter, Conn, HeadersClosed, HeadersOpen, Middleware, ResponseEnded, Header, closeHeaders, end, send, writeHeader)

headers :: forall t m req res rw c.
           (Traversable t, Monad m, ResponseWriter rw m) =>
           t Header
        -> Middleware
           m
           (Conn req { writer :: rw HeadersOpen | res } c)
           (Conn req { writer :: rw HeadersClosed | res } c)
headers hs conn = do
  traverse_ (writeOne conn) hs
  closeHeaders conn
  where
    writeOne c header = writeHeader header c

contentType :: forall m req res rw c.
               (Monad m, ResponseWriter rw m) =>
               MediaType
            -> Middleware
               m
               (Conn req { writer :: rw HeadersOpen | res } c)
               (Conn req { writer :: rw HeadersOpen | res } c)
contentType mediaType = writeHeader (Tuple "Content-Type" (unwrap mediaType))

class Response r where
  toResponse :: r -> String

instance responseString :: Response String where
  toResponse s = s

respond :: forall r m req res rw c.
           (Monad m, Response r, ResponseWriter rw m) =>
           r
        -> Middleware
           m
           (Conn req { writer :: rw HeadersClosed | res } c)
           (Conn req { writer :: rw ResponseEnded | res } c)
respond r = send (toResponse r) >=> end
