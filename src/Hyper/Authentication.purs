module Hyper.Authentication where

import Hyper.Conn (Conn, kind ResponseState)

setAuthentication :: forall a b req (res :: ResponseState -> Type) c (state :: ResponseState).
                     b
                  -> Conn req res state { authentication :: a | c }
                  -> Conn req res state { authentication :: b | c }
setAuthentication auth conn =
  conn { components { authentication = auth }}
