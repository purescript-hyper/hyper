module Main where

import Prelude
import Data.StrMap as StrMap
import Node.Buffer as Buffer
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(Just, Nothing))
import Data.MediaType.Common (textHTML)
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (class ResponseWriter, StatusLineOpen, ResponseEnded, writeStatus, writeHeader, closeHeaders, statusOK, Conn, Middleware, Port(Port))
import Hyper.HTML.DSL (p, text, html)
import Hyper.Node.Server (runServer, defaultOptions)
import Hyper.Response (respond, contentType)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(Base64, ASCII))
import Node.HTTP (HTTP)

data User = User String

setAuthentication :: forall a b req res c.
                     b
                  -> Conn req res { authentication :: a | c }
                  -> Conn req res { authentication :: b | c }
setAuthentication auth conn =
  conn { components = (conn.components { authentication = auth })}


addAuthentication :: forall m e req res c a.
                     (MonadEff (console :: CONSOLE, buffer :: BUFFER | e) m) =>
                     (Tuple String String -> m (Maybe a))
                  -> Middleware
                     m
                     (Conn
                      { headers :: StrMap.StrMap String | req }
                      res
                      { authentication :: Unit | c })
                     (Conn
                      { headers :: StrMap.StrMap String | req }
                      res
                      { authentication :: Maybe a | c })
addAuthentication mapper conn = do
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
            parts -> do
              liftEff (log (show parts))
              pure Nothing


requireAuthentication
  :: forall m req res c rw a.
     (Monad m, ResponseWriter rw m) =>
     Middleware
     m
     (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw StatusLineOpen | res }
      { authentication :: a | c })
     (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw ResponseEnded | res }
      { authentication :: a | c })
  -> Middleware
     m
     (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw StatusLineOpen | res }
      { authentication :: Maybe a | c })
     (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw ResponseEnded | res }
      { authentication :: Unit | c })
requireAuthentication mw conn =
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


main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP, buffer :: BUFFER | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

    htmlWithStatus status x =
      writeStatus status
      >=> contentType textHTML
      >=> closeHeaders
      >=> html x

    myProfilePage conn@{ components: { authentication: (User name) } } =
      htmlWithStatus
      statusOK
      (p [] (text ("You are authenticated as " <> name <> ".")))
      conn

    userFromBasicAuth (Tuple username password) =
      if username == "admin" && password == "admin"
      then pure (Just (User username))
      else pure Nothing

    app = addAuthentication userFromBasicAuth
          >=> requireAuthentication myProfilePage

  in runServer defaultOptions onListening onRequestError { authentication: unit } app
