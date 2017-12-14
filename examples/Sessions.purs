module Examples.Sessions where

import Prelude
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (textHTML)
import Hyper.Cookies (cookies)
import Hyper.Middleware (lift')
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Node.Session.InMemory (newInMemorySessionStore)
import Hyper.Request (getRequestData)
import Hyper.Response (closeHeaders, contentType, end, redirect, respond, writeStatus)
import Hyper.Session (deleteSession, getSession, saveSession)
import Hyper.Status (statusNotFound, statusOK)
import Node.HTTP (HTTP)

newtype MySession = MySession { userId :: Int }

main :: forall e. Eff (exception :: EXCEPTION, ref :: REF, console :: CONSOLE, http :: HTTP, random ::RANDOM | e) Unit
main = void $ launchAff do
  store <- liftEff newInMemorySessionStore
  liftEff (runServer defaultOptionsWithLogging (components store) app)
  where
    components store =
      { sessions: { key: "my-session"
                  , store: store
                  }
      , cookies: unit
      }

    home =
      writeStatus statusOK
      :*> contentType textHTML
      :*> closeHeaders
      :*> getSession :>>=
          case _ of
            Just (MySession { userId }) ->
              lift' (log "Session") :*>
              respond ("You are logged in as user " <> show userId <> ". "
                      <> "<a href=\"/logout\">Logout</a> if you're anxious.")
            Nothing ->
              lift' (log "No Session") :*>
              respond "<a href=\"/login\">Login</a> to start a session."

    login =
      redirect "/"
      :*> saveSession (MySession { userId: 1 })
      :*> contentType textHTML
      :*> closeHeaders
      :*> end

    logout =
      redirect "/"
      :*> deleteSession
      :*> closeHeaders
      :*> end

    notFound =
      writeStatus statusNotFound
      :*> contentType textHTML
      :*> closeHeaders
      :*> respond "Not Found"

    -- Simple router for this example.
    router =
      getRequestData :>>= \{ url } ->
      case url of
        "/" -> home
        "/login" -> login
        "/logout" -> logout
        _ -> notFound

    app =
      cookies
      :*> router
