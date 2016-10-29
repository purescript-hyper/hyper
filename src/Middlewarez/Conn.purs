module Middlewarez.Conn (
  Conn,
  Middleware,
  RequestMiddleware,
  ResponseMiddleware
  ) where

-- Might add default labels here later, like headers.
type Conn req res = { request :: req
                    , response :: res
                    }

type Middleware req req' res res' = Conn req res -> Conn req' res'
type RequestMiddleware req req' = forall res. Conn req res -> Conn req' res
type ResponseMiddleware res res' = forall req. Conn req res -> Conn req res'
