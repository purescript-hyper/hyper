module Hyper.Response where

import Prelude
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)

foreign import _respond :: forall req res c.
                           String
                        -> Conn req { body :: Unit | res } c
                        -> Conn req { body :: String | res } c

class Response r where
  toResponse :: r -> String

instance responseStringResponse :: Response String where
  toResponse s = s

respond :: forall r m req res c. (Applicative m, Response r) =>
           r
        -> Middleware m (Conn req { body :: Unit | res } c) (Conn req { body :: String | res } c)
respond r c = pure (_respond (toResponse r) c)

notFound :: forall m req res c.
            Applicative m =>
            Middleware m (Conn req { body :: Unit | res } c) (Conn req { body :: String | res } c)
notFound = respond "404 Not found"

notSupported :: forall m req res c.
                Applicative m =>
                Middleware m (Conn req { body :: Unit | res } c) (Conn req { body :: String | res } c)
notSupported = respond "405 Method not supported"
