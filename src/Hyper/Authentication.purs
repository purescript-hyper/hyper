module Hyper.Authentication where

import Data.StrMap as StrMap
import Control.Monad (class Monad)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Hyper.Core (ResponseEnded, class ResponseWriter, StatusLineOpen, Conn, Middleware)

class Authenticator a m t | a -> m where
  addAuthentication
    :: forall req res c.
       a
    -> Middleware
        m
        (Conn
         { headers :: StrMap.StrMap String | req }
         res
         { authentication :: Unit | c })
        (Conn
         { headers :: StrMap.StrMap String | req }
         res
         { authentication :: Maybe t | c })

  requireAuthentication
    :: forall req res c rw.
      (Monad m, ResponseWriter rw m) =>
      a
    -> Middleware
       m
       (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw StatusLineOpen | res }
        { authentication :: t | c })
       (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw ResponseEnded | res }
        { authentication :: t | c })
    -> Middleware
      m
      (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw StatusLineOpen | res }
        { authentication :: Maybe t | c })
      (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw ResponseEnded | res }
        { authentication :: Unit | c })

setAuthentication :: forall a b req res c.
                     b
                  -> Conn req res { authentication :: a | c }
                  -> Conn req res { authentication :: b | c }
setAuthentication auth conn =
  conn { components = (conn.components { authentication = auth })}

withAuthentication
  :: forall m req res c a t.
     Authenticator a m t =>
     Middleware
     m
     (Conn
      { headers :: StrMap.StrMap String | req }
      res
      { authentication :: Unit, authenticator :: a | c })
     (Conn
      { headers :: StrMap.StrMap String | req }
      res
      { authentication :: Maybe t, authenticator :: a | c })
withAuthentication conn =
  addAuthentication conn.components.authenticator conn

authenticated
  :: forall req res c rw a m t.
     (Monad m, ResponseWriter rw m, Authenticator a m t) =>
     Middleware
      m
      (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw StatusLineOpen | res }
      { authentication :: t, authenticator :: a | c })
      (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw ResponseEnded | res }
      { authentication :: t, authenticator :: a | c })
  -> Middleware
    m
    (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw StatusLineOpen | res }
      { authentication :: Maybe t, authenticator :: a | c })
    (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw ResponseEnded | res }
      { authentication :: Unit, authenticator :: a | c })
authenticated mw conn =
  requireAuthentication conn.components.authenticator mw conn
