module Hyper.Core where

import Data.Tuple (Tuple)
import Data.Newtype (class Newtype)

newtype Port = Port Int

derive instance newtypePort :: Newtype Port _

type Header = Tuple String String

type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }

-- | The basic middleware type for transforming a 'Conn'.
type Middleware m c c' = c -> m c'

-- Response Writer

-- | Type indicating that headers are ready to be
-- | sent, i.e. the body streaming has not been started.
data HeadersOpen = HeadersOpen

-- | Type indicating that headers have already been
-- | sent, and that the body is currently streaming.
data HeadersClosed = HeadersClosed

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
class ResponseWriter rw m | rw -> m where
  writeHeader :: Header -> ResponseStateTransition m rw HeadersOpen HeadersOpen
  closeHeaders :: ResponseStateTransition m rw HeadersOpen HeadersClosed
  send :: String -> ResponseStateTransition m rw HeadersClosed HeadersClosed
  end :: ResponseStateTransition m rw HeadersClosed ResponseEnded
