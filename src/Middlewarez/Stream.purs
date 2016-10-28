module Middlewarez.Stream where

foreign import data Stream :: * -> *

-- | Marks a stream in its initial state.
data Initial
-- | Marks an open stream.
data Open
-- | Marks a closed stream.
data Closed

-- Temporary hack.
foreign import someStream :: Stream Initial
