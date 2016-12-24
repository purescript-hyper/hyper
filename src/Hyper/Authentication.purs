module Hyper.Authentication where

import Hyper.Core (Conn)

setAuthentication :: forall a b req res c.
                     b
                  -> Conn req res { authentication :: a | c }
                  -> Conn req res { authentication :: b | c }
setAuthentication auth conn =
  conn { components = (conn.components { authentication = auth })}
