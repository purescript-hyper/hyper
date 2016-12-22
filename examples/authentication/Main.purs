module Main where

import Prelude
import Data.StrMap as StrMap
import Node.Buffer as Buffer
import Control.Monad.Aff (Aff)
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

class Authenticator a m t | a -> m where
  addAuthentication
    :: forall req res c.
       a
    -> Middleware
        m
        (Conn
         { headers :: StrMap.StrMap String | req }
         res
         { authentication :: Unit | c })
        (Conn
         { headers :: StrMap.StrMap String | req }
         res
         { authentication :: Maybe t | c })

  requireAuthentication
    :: forall req res c rw.
      (Monad m, ResponseWriter rw m) =>
      a
    -> Middleware
       m
       (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw StatusLineOpen | res }
        { authentication :: t | c })
       (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw ResponseEnded | res }
        { authentication :: t | c })
    -> Middleware
      m
      (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw StatusLineOpen | res }
        { authentication :: Maybe t | c })
      (Conn
        { headers :: StrMap.StrMap String | req }
        { writer :: rw ResponseEnded | res }
        { authentication :: Unit | c })

setAuthentication :: forall a b req res c.
                     b
                  -> Conn req res { authentication :: a | c }
                  -> Conn req res { authentication :: b | c }
setAuthentication auth conn =
  conn { components = (conn.components { authentication = auth })}

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

withAuthentication
  :: forall m e req res c a t.
     Authenticator a m t =>
     Middleware
     m
     (Conn
      { headers :: StrMap.StrMap String | req }
      res
      { authentication :: Unit, authenticator :: a | c })
     (Conn
      { headers :: StrMap.StrMap String | req }
      res
      { authentication :: Maybe t, authenticator :: a | c })
withAuthentication conn =
  addAuthentication conn.components.authenticator conn

authenticated
  :: forall req res c rw a m t.
     (Monad m, ResponseWriter rw m, Authenticator a m t) =>
     Middleware
      m
      (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw StatusLineOpen | res }
      { authentication :: t, authenticator :: a | c })
      (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw ResponseEnded | res }
      { authentication :: t, authenticator :: a | c })
  -> Middleware
    m
    (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw StatusLineOpen | res }
      { authentication :: Maybe t, authenticator :: a | c })
    (Conn
      { headers :: StrMap.StrMap String | req }
      { writer :: rw ResponseEnded | res }
      { authentication :: Unit, authenticator :: a | c })
authenticated mw conn =
  requireAuthentication conn.components.authenticator mw conn

userFromBasicAuth :: forall e. Tuple String String -> Aff e (Maybe User)
userFromBasicAuth =
  case _ of
    Tuple "admin" "admin" -> pure (Just (User "Administrator"))
    _ -> pure Nothing

myProfilePage
  :: forall m req res c rw.
     (Monad m, ResponseWriter rw m) =>
     Middleware
     m
     (Conn
      req
      { writer :: rw StatusLineOpen | res }
      { authentication :: User | c })
     (Conn
      req
      { writer :: rw ResponseEnded | res }
      { authentication :: User | c })
myProfilePage conn@{ components: { authentication: (User name) } } =
  writeStatus statusOK conn
  >>= contentType textHTML
  >>= closeHeaders
  >>= html (p [] (text ("You are authenticated as " <> name <> ".")))

main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP, buffer :: BUFFER | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

    app = withAuthentication >=> (authenticated myProfilePage)
    components =
      { authentication: unit
      , authenticator: BasicAuth userFromBasicAuth
      }
  in runServer defaultOptions onListening onRequestError components app
