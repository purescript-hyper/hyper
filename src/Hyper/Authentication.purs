module Hyper.Authentication where

import Hyper.Conn (Conn, kind ResponseState)

type AUTHENTICATION_ROWS a r = ( authentication :: a | r )

setAuthentication :: forall a b req reqState (res :: ResponseState -> Type) (resState :: ResponseState) c.
                     b
                  -> Conn req reqState res resState { | AUTHENTICATION_ROWS a c }
                  -> Conn req reqState res resState { | AUTHENTICATION_ROWS b c  }
setAuthentication auth conn =
  conn { components { authentication = auth }}
