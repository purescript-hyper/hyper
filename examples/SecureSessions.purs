module Examples.SecureSessions where

import Prelude
import Data.Map as Map
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar', modifyVar, peekVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (textHTML)
import Data.Newtype (unwrap)
import Hyper.Cookies (cookies)
import Hyper.Middleware (lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, contentType, end, redirect, respond, writeStatus)
import Hyper.Session (class SessionStore, SessionID(..), deleteSession, getSession, saveSession)
import Hyper.Status (statusNotFound, statusOK)
import Node.HTTP (HTTP)

newtype MySession = MySession { userId :: Int }

data InMemorySessionStore session = InMemorySessionStore (AVar (Map SessionID session))

instance sessionStoreInMemorySessionStore :: (Monad m, MonadAff (avar :: AVAR, console :: CONSOLE | e) m)
                                          => SessionStore
                                            (InMemorySessionStore session)
                                            m
                                            session where
  newSessionID _ =
    pure (SessionID "new-id")

  get (InMemorySessionStore var) id =
    liftAff do
      log ("Looking up session: " <> show (unwrap id))
      Map.lookup id <$> peekVar var

  put (InMemorySessionStore var) id session = do
    liftAff do
      log ("Saving session: " <> unwrap id)
      modifyVar (Map.insert id session) var

  delete (InMemorySessionStore var) id = do
    liftAff do
      log ("Deleting session: " <> unwrap id)
      modifyVar (Map.delete id) var

newInMemorySessionStore
  :: forall e session
   . Aff ( avar ∷ AVAR | e ) (InMemorySessionStore session)
newInMemorySessionStore = InMemorySessionStore <$> makeVar' Map.empty

main :: forall e. Eff (err :: EXCEPTION, avar :: AVAR, console :: CONSOLE, http :: HTTP | e) Unit
main = void $ launchAff do
  store <- newInMemorySessionStore
  liftEff (runServer defaultOptionsWithLogging (components store) app)
  where
    components store =
      { sessions: { key: "my-session"
                  , store: store
                  }
      , cookies: unit
      }
    router =
      getConn :>>= \conn ->
      case conn.request.url of
        "/" ->
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
        "/login" ->
          redirect "/"
          :*> saveSession (MySession { userId: 1 })
          :*> contentType textHTML
          :*> closeHeaders
          :*> end
        "/logout" ->
          redirect "/"
          :*> deleteSession
          :*> closeHeaders
          :*> end
        _ ->
          writeStatus statusNotFound
          :*> contentType textHTML
          :*> closeHeaders
          :*> respond "Not Found"
    app =
      cookies
      :*> router
