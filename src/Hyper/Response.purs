module Hyper.Response where

import Prelude
import Data.Foldable (traverse_)
import Data.Tuple (Tuple)
import Hyper.Core (closeHeaders, writeHeader, HeadersOpen, HeadersClosed, end, send, class ResponseWriter, ResponseEnded, Conn, Middleware)

headers :: forall m req res rw c. (Monad m, ResponseWriter rw m) =>
           Array (Tuple String String)
        -> Middleware
           m
           (Conn req { writer :: rw, state :: HeadersOpen | res } c)
           (Conn req { writer :: rw, state :: HeadersClosed | res } c)
headers hs conn = do
  traverse_ (writeOne conn) hs
  closeHeaders conn.response.writer conn
  where
    writeOne c header = writeHeader c.response.writer header c

class Response r where
  toResponse :: r -> String

instance responseStringResponse :: Response String where
  toResponse s = s

respond :: forall r m req res rw c. (Monad m, Response r, ResponseWriter rw m) =>
           r
        -> Middleware
           m
           (Conn req { writer :: rw, state :: HeadersClosed | res } c)
           (Conn req { writer :: rw, state :: ResponseEnded | res } c)
respond r c = do
  c' <- send c.response.writer (toResponse r) c
  end c.response.writer c'

notFound :: forall m req res rw c.
            (Monad m, ResponseWriter rw m) =>
            Middleware
                m
                (Conn req { writer :: rw, state :: HeadersClosed | res } c)
                (Conn req { writer :: rw, state :: ResponseEnded | res } c)
notFound = respond "404 Not found"

notSupported :: forall m req res rw c.
                (Monad m, ResponseWriter rw m) =>
                Middleware
                m
                (Conn req { writer :: rw, state :: HeadersClosed | res } c)
                (Conn req { writer :: rw, state :: ResponseEnded | res } c)
notSupported = respond "405 Method not supported"
