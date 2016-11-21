module Hyper.Conn (
  Conn,
  HTTP,
  Middleware,
  RequestMiddleware,
  ResponseMiddleware,
  PartialMiddleware,
  chain,
  (&&>),
  chainPartial,
  (??>),
  fallbackTo
  ) where

import Prelude
import Control.Monad.Aff
import Control.Bind ((>>=))
import Data.Maybe (Maybe(Nothing, Just))

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
type RequestMiddleware e req req' c =
  forall res. Middleware e req req' res res c c

-- | A middleware that only transforms the response.
type ResponseMiddleware e res res' c =
  forall req. Middleware e req req res res' c c

-- | A middleware that maybe returns a 'Just Conn', or 'Nothing'.
type PartialMiddleware e req req' res res' c c' =
  Conn req res c
  -> Aff (http :: HTTP | e) (Maybe (Conn req' res' c'))

chain :: forall e req req' req'' res res' res'' c c' c''.
         Middleware e req req' res res' c c'
      -> Middleware e req' req'' res' res'' c' c''
      -> Middleware e req req'' res res'' c c''
chain f g conn = f conn >>= g

infix 3 chain as &&>

chainPartial :: forall e req req' res res' c c'.
                PartialMiddleware e req req' res res' c c'
             -> PartialMiddleware e req req' res res' c c'
             -> PartialMiddleware e req req' res res' c c'
chainPartial f g conn = do
  result <- f conn
  case result of
    Just conn' -> pure (Just conn')
    Nothing -> g conn

infix 3 chainPartial as ??>

fallbackTo :: forall e req req' res res' c c'.
              Middleware e req req' res res' c c'
              -> PartialMiddleware e req req' res res' c c'
              -> Middleware e req req' res res' c c'
fallbackTo fallback mw conn = do
  result <- mw conn
  case result of
    Just conn' -> pure conn'
    Nothing -> fallback conn
