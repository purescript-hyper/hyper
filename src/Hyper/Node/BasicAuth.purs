module Hyper.Node.BasicAuth where

import Prelude
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Control.Monad.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect.Class (liftEffect, class MonadEffect)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object as Object
import Hyper.Authentication (setAuthentication)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class ResponseWritable, respond, class Response, ResponseEnded, StatusLineOpen, closeHeaders, writeHeader, writeStatus)
import Hyper.Status (statusUnauthorized)
import Node.Encoding (Encoding(ASCII, Base64))

type Realm = String

decodeBase64 ∷ ∀ m c
  .  MonadEffect m
  => String
  → Middleware m c c String
decodeBase64 encoded = liftEffect do
  buffer :: Buffer <- Buffer.fromString encoded Base64
  Buffer.toString ASCII buffer


withAuthentication
  :: forall m req res c t
  .  MonadEffect m
  => Request req m
  => (Tuple String String -> m (Maybe t))
  -> Middleware
     m
     (Conn req res { authentication :: Unit | c })
     (Conn req res { authentication :: Maybe t | c })
     Unit
withAuthentication mapper = Ix.do
  auth <- getAuth
  modifyConn (setAuthentication auth)
  where
    splitPair s =
      case split (Pattern ":") s of
        [username, password] -> Just (Tuple username password)
        _ -> Nothing
    getAuth = Ix.do
      { headers } <- getRequestData
      case Object.lookup "authorization" headers of
        Nothing -> ipure Nothing
        Just header -> do
          case split (Pattern " ") header of
            ["Basic", encoded] -> Ix.do
              decoded <- splitPair <$> decodeBase64 encoded
              case decoded of
                Just auth -> lift' (mapper auth)
                Nothing -> ipure Nothing
            parts -> ipure Nothing

authenticated
  :: forall m req res c b t
  .  Monad m
  => ResponseWritable b m String
  => Response res m b
  => Realm
  -> Middleware
      m
      (Conn req (res StatusLineOpen) { authentication :: t | c })
      (Conn req (res ResponseEnded) { authentication :: t | c })
      Unit
  -> Middleware
     m
     (Conn req (res StatusLineOpen) { authentication :: Maybe t | c })
     (Conn req (res ResponseEnded) { authentication :: Maybe t | c })
     Unit
authenticated realm mw = Ix.do
  conn ← getConn
  case conn.components.authentication of
    Nothing -> Ix.do
      writeStatus statusUnauthorized
      writeHeader (Tuple "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\""))
      closeHeaders
      respond "Please authenticate."
    Just auth -> Ix.do
      modifyConn (setAuthentication auth)
      mw
      modifyConn (setAuthentication (Just auth))
