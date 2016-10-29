module Middlewarez.Conn (
  Conn,
  HTTP,
  Middleware,
  RequestMiddleware,
  ResponseMiddleware
  ) where

import Control.Monad.Aff

foreign import data HTTP :: !

-- Might add default labels here later, like headers.
type Conn req res = { request :: req
                    , response :: res
                    }

type Middleware e req req' res res' = Conn req res -> Aff (http :: HTTP | e) (Conn req' res')
type RequestMiddleware e req req' = forall res. Middleware e req req' res res
type ResponseMiddleware e res res' = forall req. Middleware e req req res res'
