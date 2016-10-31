module Hyper.Router where

import Prelude
import Control.Monad.Aff (Aff)
import Hyper.Conn (Conn, ResponseMiddleware, Middleware)
import Hyper.Method (Method)
import Hyper.Stream (Initial, Stream)

{-

{
  "" : {
    "foo": {
       "bars": {
          "": {
            "POST": ...
         },
         ":id": {
           "GET": ...
         }
       }
    }
  }
}

-}

foreign import _addRoutes :: forall r req res c. r 
                         -> Conn req res { | c }
                         -> Conn req res { routes :: r | c }

foreign import _runRoutes :: forall r e req res c. r
                         -> String
                         -> Conn req { | res } c
                         -> Aff e (Conn req { body :: String | res } c)

router :: forall rs e req res c.
          rs
       -> Middleware
          e
          { method :: Method | req }
          { method :: Method | req }
          { | res }
          { body :: String | res }
          { | c }
          { routes :: rs | c }
router routes conn = 
  _addRoutes routes <$> _runRoutes routes (show conn.request.method) conn

