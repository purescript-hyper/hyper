module Hyper.Authentication where

import Hyper.Conn (Conn, kind ResponseState)

setAuthentication :: forall a b req (res :: ResponseState -> Type) c (state :: ResponseState).
                     b
                  -> Conn req res { authentication :: a | c } state
                  -> Conn req res { authentication :: b | c } state
setAuthentication auth conn =
  conn { components { authentication = auth }}
