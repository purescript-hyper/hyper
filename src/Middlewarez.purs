module Middlewarez where

import Data.Map (empty, Map)

type Headers = Map String String

foreign import data Initial :: *
foreign import data Open :: *
foreign import data Closed :: *

foreign import data Socket :: * -> *

foreign import initialSocket :: Socket Initial
          
type Connection rs = { socket :: Socket s , headers :: Headers }

test :: Connection (ResponseState Empty)
test = { responseState: emptyResponseState
       , headers: empty
       }

type Middleware s s' = Connection s -> Connection s'

open :: Connection (ResponseState Empty) -> Connection (ResponseState Partial)
open s conn = conn { responseState = emptyResponseState }

respond :: String -> Middleware (ResponseState Empty) (ResponseState Complete)
respond s conn = conn { responseState = emptyResponseState }


