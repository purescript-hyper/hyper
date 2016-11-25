module Hyper.Middleware where

import Prelude
import Control.Alt ((<|>), class Alt)
import Control.Monad.Aff (Aff)
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT)
import Data.Maybe (Maybe(Nothing, Just))
import Hyper.Conn (Conn, HTTP)

-- | The basic middleware type for transforming possibly both request and
-- | response.
type Middleware e req req' res res' c c' =
  Conn req res c -> Aff (http :: HTTP | e) (Conn req' res' c')

-- | A middleware that only transforms the request.
type RequestMiddleware e req req' c =
  forall res. Middleware e req req' res res c c

-- | A middleware that only transforms the response.
type ResponseMiddleware e res res' c =
  forall req. Middleware e req req res res' c c

type MaybeHandled e = MaybeT (Aff e)

-- | A middleware that maybe returns a 'Just Conn', or 'Nothing'.
type PartialMiddleware e req req' res res' c c' =
  Conn req res c
  -> MaybeHandled (http :: HTTP | e) (Conn req' res' c')

composeAlt :: forall m a b. Alt m => (a -> m b) -> (a -> m b) -> a -> m b
composeAlt f g x = f x <|> g x

infix 4 composeAlt as <||>

fallbackTo :: forall e req req' res res' c c'.
              Middleware e req req' res res' c c'
              -> PartialMiddleware e req req' res res' c c'
              -> Middleware e req req' res res' c c'
fallbackTo fallback mw conn = do
  result <- runMaybeT (mw conn)
  case result of
    Just conn' -> pure conn'
    Nothing -> fallback conn
