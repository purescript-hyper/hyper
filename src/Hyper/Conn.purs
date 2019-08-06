module Hyper.Conn where

import Prim.TypeError (class Fail, Text)

-- | Defines the states of an HTTP request stream. It tracks whether or not
-- | some content has already been written to an HTTP request stream.
-- |
-- | Used in combination with the `ValidResponseTransition` type class
-- | (i.e. a type-level function) to assert that a transition is valid
-- | or else compilation will fail.
foreign import kind ResponseState

-- | Type indicating that the status-line is ready to be
-- | sent.
foreign import data StatusLineOpen :: ResponseState

-- | Type indicating that headers are ready to be
-- | sent, i.e. the body streaming has not been started.
foreign import data HeadersOpen :: ResponseState

-- | Type indicating that headers have already been
-- | sent, and that the body is currently streaming.
foreign import data BodyOpen :: ResponseState

-- | Type indicating that headers have already been
-- | sent, and that the body stream, and thus the response,
-- | is finished.
foreign import data ResponseEnded :: ResponseState

-- | Type-level function that ensures a response state transition is valid
-- | (a transition always goes from the left entity to the right):
-- | StatusLineOpen -> HeadersOpen* -> BodyOpen* -> ResponseEnded
-- |
-- | *: Items marked with an asterisk indicate that transitioning
-- | back to the same state is valid.
class ValidResponseTransition (from :: ResponseState) (to :: ResponseState)

instance validStatusToHeaders :: ValidResponseTransition StatusLineOpen HeadersOpen
else instance validHeadersToHeaders :: ValidResponseTransition HeadersOpen HeadersOpen
else instance validHeadersToBody :: ValidResponseTransition HeadersOpen BodyOpen
else instance validBodyToBody :: ValidResponseTransition BodyOpen BodyOpen
else instance validBodyToEnd :: ValidResponseTransition BodyOpen ResponseEnded
else instance invalidTransition :: Fail
  (Text "Invalid response state transition") => ValidResponseTransition from to

-- | A `Conn` models the entirety of an HTTP connection, containing the fields
-- | `request`, `response`, and the extensibility point `components`.
type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }
