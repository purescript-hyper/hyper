module Examples.Sessions where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed ((:>>=))
import Effect.Aff (launchAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
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

newtype MySession = MySession { userId :: Int }

main :: Effect Unit
main = void $ launchAff do
  store <- liftEffect newInMemorySessionStore
  liftEffect (runServer defaultOptionsWithLogging (components store) app)
  where
    components store =
      { sessions: { key: "my-session"
                  , store: store
                  }
      , cookies: unit
      }

    home = Ix.do
      writeStatus statusOK
      contentType textHTML
      closeHeaders
      getSession :>>=
          case _ of
            Just (MySession { userId }) -> Ix.do
              lift' (log "Session")
              respond ("You are logged in as user " <> show userId <> ". "
                      <> "<a href=\"/logout\">Logout</a> if you're anxious.")
            Nothing -> Ix.do
              lift' (log "No Session")
              respond "<a href=\"/login\">Login</a> to start a session."

    login = Ix.do
      redirect "/"
      saveSession (MySession { userId: 1 })
      contentType textHTML
      closeHeaders
      end

    logout = Ix.do
      redirect "/"
      deleteSession
      closeHeaders
      end

    notFound = Ix.do
      writeStatus statusNotFound
      contentType textHTML
      closeHeaders
      respond "Not Found"

    -- Simple router for this example.
    router =
      getRequestData :>>= \{ url } ->
      case url of
        "/" -> home
        "/login" -> login
        "/logout" -> logout
        _ -> notFound

    app = Ix.do
      cookies
      router
