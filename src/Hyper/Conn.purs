module Hyper.Conn where

-- | A `Conn` models the entirety of an HTTP connection, containing the fields
-- | `request`, `response`, and the extensibility point `components`.
type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }
