module Hyper.Authorization where

import Control.Monad.Indexed (ibind)
import Control.Monad (class Monad)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Unit (unit, Unit)
import Hyper.Conn (Conn, ResponseEnded, StatusLineOpen, kind ResponseState)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Response (class ResponseWritable, respond, headers, writeStatus, class Response)
import Hyper.Status (statusForbidden)

withAuthorization :: forall a b req (res :: ResponseState -> Type) c (state :: ResponseState).
                     b
                  -> Conn req res { authorization :: a | c } state
                  -> Conn req res { authorization :: b | c } state
withAuthorization a conn =
  conn { components = (conn.components { authorization = a }) }


authorized :: forall a m req (res :: ResponseState -> Type) b c
  .  Monad m
  => ResponseWritable b m String
  => Response res m b
  => (Conn req res { authorization :: Unit | c } StatusLineOpen -> m (Maybe a))
  -> Middleware
     m
     (Conn req res { authorization :: a | c } StatusLineOpen)
     (Conn req res { authorization :: a | c } ResponseEnded)
     Unit
  -> Middleware
     m
     (Conn req res { authorization :: Unit | c } StatusLineOpen)
     (Conn req res { authorization :: Unit | c } ResponseEnded)
     Unit
authorized authorizer mw = do
  conn ← getConn
  auth ← lift' (authorizer conn)
  case auth of
    Just a -> do
      _ <- modifyConn (withAuthorization a)
      _ <- mw
      modifyConn (withAuthorization unit)
    Nothing -> do
      _ <- writeStatus statusForbidden
      _ <- headers []
      respond "You are not authorized."
  where bind = ibind
