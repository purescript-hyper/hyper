module Hyper.Node.BasicAuth where

import Data.StrMap as StrMap
import Node.Buffer as Buffer
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad (class Monad, (>>=))
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Data.Function ((#))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (StrMap)
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit)
import Hyper.Authentication (setAuthentication)
import Hyper.Core (class ResponseWriter, ResponseEnded, StatusLineOpen, Conn, Middleware, closeHeaders, writeHeader, writeStatus)
import Hyper.Response (respond)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(ASCII, Base64))

type Realm = String

withAuthentication
  :: forall m e req res c t.
     MonadEff (buffer :: BUFFER | e) m =>
     (Tuple String String -> m (Maybe t))
  -> Middleware
     m
     (Conn { headers :: StrMap String | req } res { authentication :: Unit | c })
     (Conn { headers :: StrMap String | req } res { authentication :: Maybe t | c })
withAuthentication mapper conn = do
  auth <- getAuth
  pure (setAuthentication auth conn)
  where
    decodeBase64 encoded =
      liftEff (Buffer.fromString encoded Base64 >>= Buffer.toString ASCII)
    splitPair s =
      case split (Pattern ":") s of
        [username, password] -> Just (Tuple username password)
        _ -> Nothing
    getAuth =
      case StrMap.lookup "authorization" conn.request.headers of
        Nothing -> pure Nothing
        Just header -> do
          case split (Pattern " ") header of
            ["Basic", encoded] -> do
              decoded <- splitPair <$> decodeBase64 encoded
              case decoded of
                Just auth -> mapper auth
                Nothing -> pure Nothing
            parts -> pure Nothing

authenticated
  :: forall m req res c rw t.
     (Monad m, ResponseWriter rw m) =>
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
authenticated realm mw conn =
  case conn.components.authentication of
    Nothing ->
      writeStatus (Tuple 401 "Unauthorized") conn
      >>= writeHeader (Tuple "WWW-Authenticate" "Basic realm=\" <> realm <> \"")
      >>= closeHeaders
      >>= respond "Please authenticate."
    Just auth ->
      setAuthentication auth conn
      # mw
      # map (setAuthentication (Just auth))
