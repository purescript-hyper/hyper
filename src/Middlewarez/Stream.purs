module Middlewarez.Stream where

foreign import data Stream :: * -> *

-- | Marks a stream in its initial state.
foreign import data Initial :: *
-- | Marks an open stream.
foreign import data Open :: *
-- | Marks a closed stream.
foreign import data Closed :: *

foreign import stdin :: Stream Initial
