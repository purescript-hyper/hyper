module Hyper.Authentication where

import Hyper.Conn (Conn, kind ResponseState)

setAuthentication :: forall a b req reqState (res :: ResponseState -> Type) (resState :: ResponseState) c.
                     b
                  -> Conn req reqState res resState { authentication :: a | c }
                  -> Conn req reqState res resState { authentication :: b | c }
setAuthentication auth conn =
  conn { components { authentication = auth }}
