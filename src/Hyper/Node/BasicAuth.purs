module Hyper.Node.BasicAuth where

import Control.Monad (class Monad, (>>=))
import Control.Monad.Indexed (ibind, ipure)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid ((<>))
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit)
import Effect.Class (liftEffect, class MonadEffect)
import Foreign.Object as Object
import Hyper.Authentication (AUTHENTICATION_ROWS, setAuthentication)
import Hyper.Conn (NoTransition', ResponseEnded, ResponseTransition, StatusLineOpen, kind ResponseState)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class ResponseWritable, respond, class Response, closeHeaders, writeHeader, writeStatus)
import Hyper.Status (statusUnauthorized)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(ASCII, Base64))

type Realm = String

decodeBase64 ∷ ∀ m c
  .  MonadEffect m
  => String
  → Middleware m c c String
decodeBase64 encoded =
  liftEffect (Buffer.fromString encoded Base64 >>= Buffer.toString ASCII)


withAuthentication
  :: forall m req reqState (res :: ResponseState -> Type) c t (resState :: ResponseState)
  .  MonadEffect m
  => Request req m
  => (Tuple String String -> m (Maybe t))
  -> NoTransition' m req reqState res resState
      { | AUTHENTICATION_ROWS Unit c }
      { | AUTHENTICATION_ROWS (Maybe t) c }
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
      { headers } <- getRequestData
      case Object.lookup "authorization" headers of
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
  :: forall m req reqState (res :: ResponseState -> Type) c b t
  .  Monad m
  => ResponseWritable b m String
  => Response res m b
  => Realm
  -> ResponseTransition m req reqState res StatusLineOpen ResponseEnded
      { | AUTHENTICATION_ROWS t c }
      Unit
  -> ResponseTransition m req reqState res StatusLineOpen ResponseEnded
     { | AUTHENTICATION_ROWS (Maybe t) c }
     Unit
authenticated realm mw = do
  conn ← getConn
  case conn.components.authentication of
    Nothing -> do
      _ <- writeStatus statusUnauthorized
      _ <- writeHeader (Tuple "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\""))
      _ <- closeHeaders
      respond "Please authenticate."
    Just auth -> do
      _ <- modifyConn (setAuthentication auth)
      _ <- mw
      modifyConn (setAuthentication (Just auth))
  where
    bind = ibind
