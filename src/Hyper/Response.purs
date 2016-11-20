module Hyper.Response where

import Prelude
import Hyper.Conn (ResponseMiddleware, Middleware, Conn)

foreign import _respond :: forall req res c.
                           String
                        -> Conn req { | res } c
                        -> Conn req { body :: String | res } c

class Response r where
  toResponse :: r -> String

instance responseStringResponse :: Response String where
  toResponse s = s

respond :: forall r e res c. Response r =>
           r
        -> ResponseMiddleware e { | res } { body :: String | res } c

respond r c = pure (_respond (toResponse r) c)

notFound :: forall e res c. ResponseMiddleware e { | res } { body :: String | res } c
notFound = respond "404 Not found"

notSupported :: forall e res c. ResponseMiddleware e { | res } { body :: String | res } c
notSupported = respond "405 Method not supported"
