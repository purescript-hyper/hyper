module Hyper.Middleware where

-- | The basic middleware type for transforming a 'Conn'.
type Middleware m c c' = c -> m c'
