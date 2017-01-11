module Hyper.Core where

import Control.Alt (class Alt)
import Control.Applicative (pure)
import Control.Monad (class Monad, bind)
import Data.Function ((<<<), ($))
import Data.Functor (map, class Functor)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(Tuple))

newtype Port = Port Int

derive instance newtypePort :: Newtype Port _

type Status = Tuple Int String

statusOK :: Status
statusOK = Tuple 200 "OK"

statusCreated :: Status
statusCreated = Tuple 201 "Created"

statusFound :: Status
statusFound = Tuple 302 "Found"

statusBadRequest :: Status
statusBadRequest = Tuple 400 "Bad Request"

statusNotFound :: Status
statusNotFound = Tuple 404 "Not Found"

statusMethodNotAllowed :: Status
statusMethodNotAllowed = Tuple 405 "Method Not Allowed"

type Header = Tuple String String

type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }

-- | The basic middleware type for transforming a 'Conn'.
type Middleware m c c' = c -> m c'

-- Response Writer

-- | Type indicating that the status-line is ready to be
-- | sent.
data StatusLineOpen = StatusLineOpen

-- | Type indicating that headers are ready to be
-- | sent, i.e. the body streaming has not been started.
data HeadersOpen = HeadersOpen

-- | Type indicating that headers have already been
-- | sent, and that the body is currently streaming.
data BodyOpen = BodyOpen

-- | Type indicating that headers have already been
-- | sent, and that the body stream, and thus the response,
-- | is finished.
data ResponseEnded = ResponseEnded

-- | A middleware transitioning from one `ResponseWriter` state to another.
type ResponseStateTransition m rw from to =
  forall req res c.
  Middleware
  m
  (Conn req {writer :: rw from | res} c)
  (Conn req {writer :: rw to | res} c)

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class ResponseWriter rw m b | rw -> b where
  writeStatus :: Status -> ResponseStateTransition m rw StatusLineOpen HeadersOpen
  writeHeader :: Header -> ResponseStateTransition m rw HeadersOpen HeadersOpen
  closeHeaders :: ResponseStateTransition m rw HeadersOpen BodyOpen
  send :: b -> ResponseStateTransition m rw BodyOpen BodyOpen
  end :: ResponseStateTransition m rw BodyOpen ResponseEnded

newtype TryMiddleware m c c' = TryMiddleware (Middleware m c (Maybe c'))

instance functorTryMiddleware :: Functor m => Functor (TryMiddleware m c) where
  map f (TryMiddleware mw) = TryMiddleware (map (map f) <<< mw)

instance altTryMiddleware :: Monad m => Alt (TryMiddleware m c) where
  -- NOTE: We have strict evaluation, and we only want to run 'm2' if 'm1'
  -- resulted in a `Nothing`.
  alt (TryMiddleware m1) (TryMiddleware m2) =
    TryMiddleware $ \conn -> do
      result <- m1 conn
      case result of
        Just conn' -> pure (Just conn')
        Nothing -> m2 conn

try
  :: forall m c c'.
     Middleware m c (Maybe c')
  -> TryMiddleware m c c'
try = TryMiddleware

fallbackTo
  :: forall m c c'.
     Monad m =>
     Middleware m c c'
  -> TryMiddleware m c c'
  -> Middleware m c c'
fallbackTo fallback (TryMiddleware mw) conn = do
  result <- mw conn
  case result of
    Just conn' -> pure conn'
    Nothing -> fallback conn
