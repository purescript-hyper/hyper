module Hyper.Middleware where

-- | The basic middleware type for transforming a conn.
type Middleware m c c' = c -> m c'
