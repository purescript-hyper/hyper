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
import Data.StrMap as StrMap
import Hyper.Cookies as Cookies
import Control.IxMonad (ibind, ipure, (:>>=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap)
import Hyper.Conn (Conn)
import Hyper.Cookies (defaultCookieAttributes, setCookie, SameSite(Lax))
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Response (class Response, HeadersOpen)

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

currentSessionID
  :: forall m req res c store session
  .  Monad m
  => SessionStore store m session
  => Middleware
     m
     (Conn
      req
      res
      { sessions :: Sessions store
      , cookies :: Either String (StrMap Cookies.Values)
      | c
      })
     (Conn
      req
      res
      { sessions :: Sessions store
      , cookies :: Either String (StrMap Cookies.Values)
      | c
      })
     (Maybe SessionID)
currentSessionID =
  getConn :>>= \conn ->
  case conn.components.cookies of
    Left err ->
      ipure Nothing
    Right cookies ->
      StrMap.lookup conn.components.sessions.key cookies
      # map (SessionID <<< NonEmpty.head)
      # pure

getSession
  :: forall m req res c store session
  .  Monad m
  => SessionStore store m session
  => Middleware
     m
     (Conn
      req
      res
      { sessions :: Sessions store
      , cookies :: Either String (StrMap Cookies.Values)
      | c
      })
     (Conn
      req
      res
      { sessions :: Sessions store
      , cookies :: Either String (StrMap Cookies.Values)
      | c
      })
     (Maybe session)
getSession = do
  conn <- getConn
  sessionId <- currentSessionID
  case sessionId of
    Just id' -> lift' (get conn.components.sessions.store id')
    Nothing -> ipure Nothing
  where bind = ibind

saveSession
  :: forall m req res c b store session
  .  Monad m
  => Response res m b
  => SessionStore store m session
  => session
  -> Middleware
     m
     (Conn
      req
      (res HeadersOpen)
      { sessions :: Sessions store, cookies :: Either String (StrMap Cookies.Values) | c})
     (Conn
      req
      (res HeadersOpen)
      { sessions :: Sessions store, cookies :: Either String (StrMap Cookies.Values) | c})
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
  :: forall m req res c b store session
  .  Monad m
  => Response res m b
  => SessionStore store m session
  => Middleware
     m
     (Conn
      req
      (res HeadersOpen)
      { sessions :: Sessions store, cookies :: Either String (StrMap Cookies.Values) | c})
     (Conn
      req
      (res HeadersOpen)
      { sessions :: Sessions store, cookies :: Either String (StrMap Cookies.Values) | c})
     Unit
deleteSession = do
  conn <- getConn
  _ <- maybe (ipure unit) (lift' <<< delete conn.components.sessions.store) <$> currentSessionID
  -- TODO: Better delete?
  setCookie conn.components.sessions.key "" defaultCookieAttributes
