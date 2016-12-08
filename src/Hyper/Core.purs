module Hyper.Core where

import Data.Tuple

type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }

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

-- | The basic middleware type for transforming a 'Conn'.
type Middleware m c c' = c -> m c'


type ResponseStateTransition m from to =
  forall req res c.
  Middleware
  m
  (Conn req {state :: from | res} c)
  (Conn req {state :: to | res} c)

class ResponseWriter rw m | rw -> m where
  writeHeader :: rw
                 -> Tuple String String
                 -> ResponseStateTransition m HeadersOpen HeadersOpen

  closeHeaders :: rw -> ResponseStateTransition m HeadersOpen HeadersClosed

  send :: rw -> String -> ResponseStateTransition m HeadersClosed HeadersClosed
  end :: rw -> ResponseStateTransition m HeadersClosed ResponseEnded
