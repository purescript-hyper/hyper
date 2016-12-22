module Hyper.Node.BasicAuth where

import Data.StrMap as StrMap
import Node.Buffer as Buffer
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad ((>>=))
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Data.Function ((#))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit)
import Hyper.Authentication (setAuthentication, class Authenticator)
import Hyper.Core (closeHeaders, writeHeader, writeStatus)
import Hyper.Response (respond)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(ASCII, Base64))

data BasicAuth m t = BasicAuth (Tuple String String -> m (Maybe t))

instance authenticatorBasicAuth :: (MonadEff (buffer :: BUFFER | e) m) =>
                                   Authenticator (BasicAuth m t) m t where
  addAuthentication (BasicAuth mapper) conn = do
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

  requireAuthentication (BasicAuth _) mw conn =
    case conn.components.authentication of
      Nothing ->
        writeStatus (Tuple 401 "Unauthorized") (setAuthentication unit conn)
        >>= writeHeader (Tuple "WWW-Authenticate" "Basic realm=\"Hyper Authentication Example\"")
        >>= closeHeaders
        >>= respond "Please authenticate."
      Just auth ->
        setAuthentication auth conn
        # mw
        # map (setAuthentication unit)
