module Hyper.Middleware where

import Prelude
import Control.Alt ((<|>), class Alt)
import Control.Monad.Aff (Aff)
import Hyper.Conn (Conn)

newtype MiddlewareT m c c' = MiddlewareT (c -> m c')

runMiddlewareT :: forall m c c'. MiddlewareT m c c' -> c -> m c'
runMiddlewareT (MiddlewareT m) x = m x

instance functorMiddleware :: Functor m => Functor (MiddlewareT m c) where
  map f (MiddlewareT m) = MiddlewareT (\conn -> f <$> m conn)

instance altMiddlewareT :: (Alt m) => Alt (MiddlewareT m c) where
  alt (MiddlewareT m) (MiddlewareT n) =
    MiddlewareT (\conn -> m conn <|> n conn)

-- | The basic middleware type for transforming a conn.
type Middleware e c c' = MiddlewareT (Aff e) c c'

-- | A middleware that only transforms the request.
type RequestMiddleware e req req' c =
  forall res. MiddlewareT (Aff e) (Conn req res c) (Conn req' res c)

-- | A middleware that only transforms the response.
type ResponseMiddleware e res res' c =
  forall req. MiddlewareT (Aff e) (Conn req res c) (Conn req res' c)
