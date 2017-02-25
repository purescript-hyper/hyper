module Hyper.Request
  ( class RequestBodyReader
  , readBody
  ) where

import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)

-- | A RequestBodyReader instance reads the request body for a specific body
-- | reader type, and transitions the state of body reading forward.
class RequestBodyReader r m b | r -> b where
  readBody
    :: forall req res c.
       Middleware
       m
       (Conn { body :: r | req } res c)
       (Conn { body :: r | req } res c)
       b
