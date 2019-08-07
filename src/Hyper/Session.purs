module Hyper.Session
       ( SessionID(..)
       , class SessionStore
       , newSessionID
       , get
       , put
       , delete
       , saveSession
       , getSession
       , deleteSession
       ) where

import Prelude
import Data.NonEmpty as NonEmpty
import Foreign.Object as Object
import Foreign.Object (Object)
import Hyper.Cookies as Cookies
import Control.Monad.Indexed (ibind, ipure, (:>>=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype, unwrap)
import Hyper.Conn (Conn, kind ResponseState, HeadersOpen)
import Hyper.Cookies (defaultCookieAttributes, maxAge, setCookie, SameSite(Lax))
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Response (class Response)

newtype SessionID = SessionID String

derive instance eqSessionID :: Eq SessionID
derive instance ordSessionID :: Ord SessionID
derive instance newtypeSessionID :: Newtype SessionID _

class SessionStore store m session | store -> m, store -> session where
  newSessionID :: store -> m SessionID
  get :: store -> SessionID -> m (Maybe session)
  put :: store -> SessionID -> session -> m Unit
  delete :: store -> SessionID -> m Unit

type Sessions s = { key :: String, store :: s }

type SESSION_COOKIE_ROWS store compRows =
    ( sessions :: Sessions store
    , cookies :: Either String (Object Cookies.Values)
    | compRows
    )

currentSessionID
  :: forall m req reqState (res :: ResponseState -> Type) c store session (resState :: ResponseState)
  .  Monad m
  => SessionStore store m session
  => Middleware
     m
     (Conn req reqState res resState { | SESSION_COOKIE_ROWS store c })
     (Conn req reqState res resState { | SESSION_COOKIE_ROWS store c })
     (Maybe SessionID)
currentSessionID =
  getConn :>>= \conn ->
  case conn.components.cookies of
    Left err ->
      ipure Nothing
    Right cookies ->
      Object.lookup conn.components.sessions.key cookies
      # map (SessionID <<< NonEmpty.head)
      # pure

getSession
  :: forall m req reqState (res :: ResponseState -> Type) c store session (resState :: ResponseState)
  .  Monad m
  => SessionStore store m session
  => Middleware
     m
     (Conn req reqState res resState { | SESSION_COOKIE_ROWS store c })
     (Conn req reqState res resState { | SESSION_COOKIE_ROWS store c })
     (Maybe session)
getSession = do
  conn <- getConn
  sessionId <- currentSessionID
  case sessionId of
    Just id' -> lift' (get conn.components.sessions.store id')
    Nothing -> ipure Nothing
  where bind = ibind

saveSession
  :: forall m req reqState (res :: ResponseState -> Type) c b store session
  .  Monad m
  => Response res m b
  => SessionStore store m session
  => session
  -> Middleware
     m
      (Conn req reqState res HeadersOpen { | SESSION_COOKIE_ROWS store c })
      (Conn req reqState res HeadersOpen { | SESSION_COOKIE_ROWS store c })
     Unit
saveSession session = do
  conn <- getConn
  sessionId <-
    currentSessionID :>>=
    case _ of
      Just id'
        | unwrap id' /= "" -> ipure id'
        | otherwise -> lift' (newSessionID conn.components.sessions.store)
      Nothing -> lift' (newSessionID conn.components.sessions.store)
  lift' (put conn.components.sessions.store sessionId session)
  setCookie
    conn.components.sessions.key
    (unwrap sessionId)
    (defaultCookieAttributes { sameSite=Just Lax, httpOnly=true })
  where
    bind = ibind

deleteSession
  :: forall m req reqState (res :: ResponseState -> Type) c b store session
  .  Monad m
  => Response res m b
  => SessionStore store m session
  => Middleware
     m
     (Conn req reqState res HeadersOpen { | SESSION_COOKIE_ROWS store c })
     (Conn req reqState res HeadersOpen { | SESSION_COOKIE_ROWS store c })
     Unit
deleteSession = do
  conn <- getConn
  _ <- maybe (ipure unit) (lift' <<< delete conn.components.sessions.store) <$> currentSessionID
  -- TODO: Better delete?
  setCookie conn.components.sessions.key "" (defaultCookieAttributes { maxAge=maxAge 0 })
