module Hyper.Node.BasicAuth where

import Data.StrMap as StrMap
import Node.Buffer as Buffer
import Control.IxMonad (ibind, ipure)
import Control.Monad (class Monad, (>>=))
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid ((<>))
import Data.StrMap (StrMap)
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit)
import Hyper.Authentication (setAuthentication)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Response (class Response, respond, class ResponseWriter, ResponseEnded, StatusLineOpen, closeHeaders, writeHeader, writeStatus)
import Hyper.Status (statusUnauthorized)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(ASCII, Base64))

type Realm = String

decodeBase64 ∷ ∀ m e c.
  MonadEff (buffer ∷ BUFFER | e) m
  ⇒ String
  → Middleware m c c String
decodeBase64 encoded =
  liftEff (Buffer.fromString encoded Base64 >>= Buffer.toString ASCII)


withAuthentication
  :: forall m e req res c t.
     MonadEff (buffer :: BUFFER | e) m =>
     (Tuple String String -> m (Maybe t))
  -> Middleware
     m
     (Conn { headers :: StrMap String | req } res { authentication :: Unit | c })
     (Conn { headers :: StrMap String | req } res { authentication :: Maybe t | c })
     Unit
withAuthentication mapper = do
  auth <- getAuth
  modifyConn (setAuthentication auth)
  where
    splitPair s =
      case split (Pattern ":") s of
        [username, password] -> Just (Tuple username password)
        _ -> Nothing
    getAuth = do
      headers ← _.request.headers <$> getConn
      case StrMap.lookup "authorization" headers of
        Nothing -> ipure Nothing
        Just header -> do
          case split (Pattern " ") header of
            ["Basic", encoded] -> do
              decoded <- splitPair <$> decodeBase64 encoded
              case decoded of
                Just auth -> lift' (mapper auth)
                Nothing -> ipure Nothing
            parts -> ipure Nothing
    bind = ibind

authenticated
  :: forall m req res c rw b t.
     (Monad m, Response b m String, ResponseWriter rw m b) =>
     Realm
  -> Middleware
      m
      (Conn
       req
       { writer :: rw StatusLineOpen | res }
       { authentication :: t | c })
      (Conn
       req
       { writer :: rw ResponseEnded | res }
       { authentication :: t | c })
      Unit
  -> Middleware
     m
     (Conn
      req
      { writer :: rw StatusLineOpen | res }
      { authentication :: Maybe t | c })
     (Conn
      req
      { writer :: rw ResponseEnded | res }
      { authentication :: Maybe t | c })
     Unit

authenticated realm mw = do
  conn ← getConn
  case conn.components.authentication of
    Nothing -> do
      writeStatus statusUnauthorized
      writeHeader (Tuple "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\""))
      closeHeaders
      respond "Please authenticate."
    Just auth -> do
      modifyConn (setAuthentication auth)
      mw
      modifyConn (setAuthentication (Just auth))
  where
    bind = ibind
