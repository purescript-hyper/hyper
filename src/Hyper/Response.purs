module Hyper.Response where

import Prelude
import Hyper.Conn (Conn, ResponseMiddleware)

foreign import _respond :: forall req res c.
                           String
                        -> Conn req { | res } c
                        -> Conn req { body :: String | res } c

class Response r where
  toResponse :: r -> String

newtype StringResponse = StringResponse String

instance responseStringResponse :: Response StringResponse where
  toResponse (StringResponse s) = s

respond :: forall r e res c. Response r =>
           r
        -> ResponseMiddleware e { | res } { body :: String | res } c
respond r c = pure (_respond (toResponse r) c)
