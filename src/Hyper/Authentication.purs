module Hyper.Authentication where

import Hyper.Conn (Conn, kind ResponseState)

setAuthentication :: forall a b req (res :: ResponseState -> Type) c (resState :: ResponseState).
                     b
                  -> Conn req res resState { authentication :: a | c }
                  -> Conn req res resState { authentication :: b | c }
setAuthentication auth conn =
  conn { components { authentication = auth }}
