module Hyper.Core where

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Unit (Unit)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Status (Status)

newtype Port = Port Int

derive instance newtypePort :: Newtype Port _

type Header = Tuple String String

-- Response Writer

-- | Type indicating that the status-line is ready to be
-- | sent.
data StatusLineOpen

-- | Type indicating that headers are ready to be
-- | sent, i.e. the body streaming has not been started.
data HeadersOpen

-- | Type indicating that headers have already been
-- | sent, and that the body is currently streaming.
data BodyOpen

-- | Type indicating that headers have already been
-- | sent, and that the body stream, and thus the response,
-- | is finished.
data ResponseEnded


-- | A middleware transitioning from one `ResponseWriter` state to another.
type ResponseStateTransition m rw from to =
  forall req res c.
  Middleware
  m
  (Conn req {writer :: rw from | res} c)
  (Conn req {writer :: rw to | res} c)
  Unit

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class ResponseWriter rw m b | rw -> b where
  writeStatus :: Status -> ResponseStateTransition m rw StatusLineOpen HeadersOpen
  writeHeader :: Header -> ResponseStateTransition m rw HeadersOpen HeadersOpen
  closeHeaders :: ResponseStateTransition m rw HeadersOpen BodyOpen
  send :: b -> ResponseStateTransition m rw BodyOpen BodyOpen
  end :: ResponseStateTransition m rw BodyOpen ResponseEnded
