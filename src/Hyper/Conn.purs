module Hyper.Conn (
  Conn,
  HTTP,
  Middleware,
  RequestMiddleware,
  ResponseMiddleware
  ) where

import Control.Monad.Aff

foreign import data HTTP :: !

type Conn req res components = { request :: req
                               , response :: res
                               , components :: components
                               }

-- | The basic middleware type for transforming possibly both request and
-- | response.
type Middleware e req req' res res' c c' =
  Conn req res c -> Aff (http :: HTTP | e) (Conn req' res' c')

-- | A middleware that only transforms the request.
type RequestMiddleware e req req' = 
  forall res c. Middleware e req req' res res c c

-- | A middleware that only transforms the response.
type ResponseMiddleware e res res' =
  forall req c. Middleware e req req res res' c c
