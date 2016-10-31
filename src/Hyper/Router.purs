module Hyper.Router where

import Prelude
import Hyper.Conn (Conn, ResponseMiddleware, Middleware)
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

foreign import _addRoutes :: forall r c. r 
                         -> { | c }
                         -> { routes :: r | c }

foreign import _runRoutes :: forall r req res c. r 
                         -> Conn req { | res } c
                         -> Conn req { body :: String | res } c

router :: forall rs e req res c.
          rs
       -> Middleware
          e
          req
          req
          { | res }
          { body :: String | res }
          { | c }
          { routes :: rs | c }
router routes conn@{ response: r, components: c } = do
  let conn' = _runRoutes routes conn
  pure (conn' { components = (_addRoutes routes c) })

type Handler e res c =
  ResponseMiddleware
  e
  { | res }
  { body :: String | res }
  c
