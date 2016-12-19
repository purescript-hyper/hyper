module Hyper.Core where

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(Tuple))

newtype Port = Port Int

derive instance newtypePort :: Newtype Port _

type Status = Tuple Int String

statusOK :: Status
statusOK = Tuple 200 "OK"

statusCreated :: Status
statusCreated = Tuple 201 "Created"

statusBadRequest :: Status
statusBadRequest = Tuple 400 "Bad Request"

statusNotFound :: Status
statusNotFound = Tuple 404 "Not Found"

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
class ResponseWriter rw m | rw -> m where
  writeStatus :: Status -> ResponseStateTransition m rw StatusLineOpen HeadersOpen
  writeHeader :: Header -> ResponseStateTransition m rw HeadersOpen HeadersOpen
  closeHeaders :: ResponseStateTransition m rw HeadersOpen BodyOpen
  send :: String -> ResponseStateTransition m rw BodyOpen BodyOpen
  end :: ResponseStateTransition m rw BodyOpen ResponseEnded
