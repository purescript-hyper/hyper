module ReadBody where

import Prelude
import Control.IxMonad ((:>>=), (:*>))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Middleware.Class (getConn)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (class RequestBodyReader, readBody)
import Hyper.Response (class Response, class ResponseWriter, ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed)
import Node.HTTP (HTTP)

onPost
  :: forall m r rw b req res c.
     ( Monad m
     , RequestBodyReader r m String
     , ResponseWriter rw m b
     , Response b m String
     )
  => Middleware
     m
     (Conn { body :: r | req } { writer :: rw StatusLineOpen | res } c)
     (Conn { body :: r | req } { writer :: rw ResponseEnded | res } c)
     Unit
-- start snippet onPost
onPost =
  readBody :>>=
  case _ of
    "" ->
      writeStatus statusBadRequest
      :*> closeHeaders
      :*> respond "... anyone there?"
    msg ->
      writeStatus statusBadRequest
      :*> closeHeaders
      :*> respond ("You said: " <> msg)
-- end snippet onPost

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR | e) Unit
main =
  let
    router =
      getConn :>>= \conn →
      case conn.request.method of
        Left POST -> onPost
        Left method ->
          writeStatus statusMethodNotAllowed
          :*> closeHeaders
          :*> respond ("Method not supported: " <> show method)
        Right customMethod ->
          writeStatus statusMethodNotAllowed
          :*> closeHeaders
          :*> respond ("Custom method not supported: " <> show customMethod)

  -- Let's run it.
  in runServer defaultOptionsWithLogging {} router
