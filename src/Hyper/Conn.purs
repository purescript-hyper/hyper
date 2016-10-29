module Hyper.Conn (
  Conn,
  HTTP,
  Middleware,
  RequestMiddleware,
  ResponseMiddleware
  ) where

import Control.Monad.Aff

foreign import data HTTP :: !

type Conn req res = { request :: req
                    , response :: res
                    }

-- | The basic middleware type for transforming possibly both request and response.
type Middleware e req req' res res' = Conn req res -> Aff (http :: HTTP | e) (Conn req' res')

-- | A middleware that only transforms the request.
type RequestMiddleware e req req' = forall res. Middleware e req req' res res

-- | A middleware that only transforms the response.
type ResponseMiddleware e res res' = forall req. Middleware e req req res res'
