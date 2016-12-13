module Hyper.BodyParser where

import Hyper.Core (Conn, Middleware)

class BodyParser p m from to | p → to where
  parse :: forall req res c h. p
        -> Middleware
           m
           (Conn
            { body :: from
            , headers :: { "content-type" :: String
                         , "content-length" :: String
                         | h
                         }
            | req
            }
            res
            c)
           (Conn
            { headers :: { "content-type" :: String
                         , "content-length" :: String
                         | h
                         }
            , body :: to
            | req
            }
            res
            c)
