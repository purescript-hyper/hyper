module Hyper.Authorization where

import Control.Bind ((>>=))
import Control.Monad (class Monad)
import Data.Function ((#))
import Data.Functor (map)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit, Unit)
import Hyper.Core (writeStatus, class ResponseWriter, Middleware, ResponseEnded, StatusLineOpen, Conn)
import Hyper.Response (respond, headers)

withAuthorization :: forall a b req res c.
                     b
                  -> Conn req res { authorization :: a | c }
                  -> Conn req res { authorization :: b | c }
withAuthorization a conn =
  conn { components = (conn.components { authorization = a }) }


authorized
  :: forall a m req res rw c.
     (Monad m, ResponseWriter rw m) =>
     (Conn req { writer :: rw StatusLineOpen | res } { authorization :: Unit | c } -> m (Maybe a))
  -> (Middleware
      m
      (Conn req { writer :: rw StatusLineOpen | res } { authorization :: a | c })
      (Conn req { writer :: rw ResponseEnded | res } { authorization :: a | c }))
  -> Middleware
     m
     (Conn req { writer :: rw StatusLineOpen | res } { authorization :: Unit | c })
     (Conn req { writer :: rw ResponseEnded | res } { authorization :: Unit | c })
authorized authorizer mw conn =
  authorizer conn >>= continue
  where
    continue =
      case _ of
        Just a ->
          conn
          # withAuthorization a
          # mw
          # map (withAuthorization unit)
        Nothing ->
          writeStatus (Tuple 403 "Forbidden") conn
          >>= headers []
          >>= respond "You are not authorized."
