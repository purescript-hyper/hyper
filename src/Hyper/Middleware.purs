module Hyper.Middleware where

import Control.Monad.Aff (Aff)
import Hyper.Conn (Conn)

type MiddlewareT m c c' = c -> m c'

-- | The basic middleware type for transforming a conn.
type Middleware e c c' = MiddlewareT (Aff e) c c'

-- | A middleware that only transforms the request.
type RequestMiddleware e req req' c =
  forall res. MiddlewareT (Aff e) (Conn req res c) (Conn req' res c)

-- | A middleware that only transforms the response.
type ResponseMiddleware e res res' c =
  forall req. MiddlewareT (Aff e) (Conn req res c) (Conn req res' c)
