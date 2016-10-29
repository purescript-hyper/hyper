module Middlewarez.Stream where

foreign import data Stream :: * -> *

-- | Marks a stream in its initial state.
data Initial
-- | Marks an open stream.
data Open
-- | Marks a closed stream.
data Closed

foreign import fromString :: String -> Stream Initial
