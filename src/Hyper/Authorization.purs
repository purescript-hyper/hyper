module Hyper.Authorization where

import Control.Monad (class Monad)
import Control.Monad.Indexed (ibind)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Unit (unit, Unit)
import Hyper.Conn (Conn, ResponseEnded, StatusLineOpen, ResponseTransition, kind ResponseState)
import Hyper.Middleware (lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Response (class ResponseWritable, respond, headers, writeStatus, class Response)
import Hyper.Status (statusForbidden)

type AUTHORIZATION_ROWS a r = ( authorization :: a | r )

withAuthorization :: forall a b req reqState (res :: ResponseState -> Type) (resState :: ResponseState) c.
                     b
                  -> Conn req reqState res resState { | AUTHORIZATION_ROWS a c }
                  -> Conn req reqState res resState { | AUTHORIZATION_ROWS b c }
withAuthorization a conn =
  conn { components = (conn.components { authorization = a }) }


authorized :: forall a m req reqState (res :: ResponseState -> Type) b c
  .  Monad m
  => ResponseWritable b m String
  => Response res m b
  => (Conn req reqState res StatusLineOpen { | AUTHORIZATION_ROWS Unit c } -> m (Maybe a))
  -> ResponseTransition m req reqState res
        StatusLineOpen ResponseEnded { | AUTHORIZATION_ROWS a c } Unit
  -> ResponseTransition m req reqState res
        StatusLineOpen ResponseEnded { | AUTHORIZATION_ROWS Unit c } Unit
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
