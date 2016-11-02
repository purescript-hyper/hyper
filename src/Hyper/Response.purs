module Hyper.Response where

import Control.Monad (pure)
import Hyper.Conn (Conn, ResponseMiddleware)

foreign import _respond :: forall req res b c.
                           b
                        -> Conn req { | res } c
                        -> Conn req { body :: b | res } c

respond :: forall e res b c. 
           b
        -> ResponseMiddleware e { | res } { body :: b | res } c
respond b c = pure (_respond b c)
