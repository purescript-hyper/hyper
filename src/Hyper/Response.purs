module Hyper.Response where

import Prelude
import Data.Foldable (traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (class ResponseWriter, Conn, BodyOpen, HeadersOpen, Middleware, ResponseEnded, Header, closeHeaders, end, send, writeHeader)

headers :: forall t m req res rw b c.
           (Traversable t, Monad m, ResponseWriter rw b m) =>
           t Header
        -> Middleware
           m
           (Conn req { writer :: rw HeadersOpen | res } c)
           (Conn req { writer :: rw BodyOpen | res } c)
headers hs conn = do
  traverse_ (writeOne conn) hs
  closeHeaders conn
  where
    writeOne c header = writeHeader header c

contentType :: forall m req res rw b c.
               (Monad m, ResponseWriter rw b m) =>
               MediaType
            -> Middleware
               m
               (Conn req { writer :: rw HeadersOpen | res } c)
               (Conn req { writer :: rw HeadersOpen | res } c)
contentType mediaType = writeHeader (Tuple "Content-Type" (unwrap mediaType))

class Response m r t | r -> t where
  toResponse :: r -> m t

instance responseString :: Monad m => Response m String String where
  toResponse = pure <<< id

respond :: forall r m req res rw b c.
           (Monad m, Response m r b, ResponseWriter rw b m) =>
           r
        -> Middleware
           m
           (Conn req { writer :: rw BodyOpen | res } c)
           (Conn req { writer :: rw ResponseEnded | res } c)
respond r conn = do
  body <- toResponse r
  send body conn >>= end
