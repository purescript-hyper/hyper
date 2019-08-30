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
import Control.Monad.Indexed (ibind, ipure, (:>>=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype, unwrap)
import Hyper.Conn (ComponentChange, HeadersOpen, kind ResponseState)
import Hyper.Cookies (COOKIES_ROWS', defaultCookieAttributes, maxAge, setCookie, SameSite(Lax))
import Hyper.Middleware (lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Response (class Response)
import Type.Row (type (+))

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

type SESSION_ROWS store r = ( sessions :: Sessions store | r)

currentSessionID
  :: forall m req reqState (res :: ResponseState -> Type) c store session (resState :: ResponseState)
  .  Monad m
  => SessionStore store m session
  => ComponentChange m req reqState res resState
      { | SESSION_ROWS store + COOKIES_ROWS' c }
      { | SESSION_ROWS store + COOKIES_ROWS' c }
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
  => ComponentChange m req reqState res resState
      { | SESSION_ROWS store + COOKIES_ROWS' c }
      { | SESSION_ROWS store + COOKIES_ROWS' c }
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
  -> ComponentChange m req reqState res HeadersOpen
      { | SESSION_ROWS store + COOKIES_ROWS' c }
      { | SESSION_ROWS store + COOKIES_ROWS' c }
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
  => ComponentChange m req reqState res HeadersOpen
      { | SESSION_ROWS store + COOKIES_ROWS' c }
      { | SESSION_ROWS store + COOKIES_ROWS' c }
      Unit
deleteSession = do
  conn <- getConn
  _ <- maybe (ipure unit) (lift' <<< delete conn.components.sessions.store) <$> currentSessionID
  -- TODO: Better delete?
  setCookie conn.components.sessions.key "" (defaultCookieAttributes { maxAge=maxAge 0 })
