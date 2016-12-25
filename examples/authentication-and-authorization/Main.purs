-- This is a bit bigger example, featuring _authentication_ and
-- _authorization_, illustrating the parts that can be custom
-- to your application, and how you can leverage the type system
-- to make sure authorization is properly checked.
module Main where

import Prelude
import Hyper.Node.BasicAuth as BasicAuth
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(Nothing, Just))
import Data.MediaType.Common (textHTML)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Authentication (setAuthentication)
import Hyper.Core (writeStatus, Status, StatusLineOpen, statusOK, statusNotFound, class ResponseWriter, ResponseEnded, Conn, Middleware, closeHeaders, Port(Port))
import Hyper.HTML.DSL (HTML, li, ul, linkTo, h1, p, text, html)
import Hyper.Method (Method)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (headers, respond, contentType)
import Hyper.Router (notSupported, resource, fallbackTo, handler)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)


-- Helper for responding with HTML.
htmlWithStatus
  :: forall m req res rw c.
     (Monad m, ResponseWriter rw m) =>
     Status
  -> HTML Unit
  -> Middleware
     m
     (Conn { url :: String, method :: Method | req }
           { writer :: rw StatusLineOpen | res }
           c)
     (Conn { url :: String, method :: Method | req }
           { writer :: rw ResponseEnded | res }
           c)
htmlWithStatus status x =
  writeStatus status
  >=> contentType textHTML
  >=> closeHeaders
  >=> html x


-- Users have user names.
type Name = String
data User = User Name


-- Authorization is specific to this example, and wraps the authenticated
-- User, in conn.components.authentication, with an Authorized value holding
-- the role.
data Admin = Admin
data Authorized r = Authorized User r


-- This could be a function checking the username/password in a database.
userFromBasicAuth :: forall e. Tuple String String -> Aff e (Maybe User)
userFromBasicAuth =
  case _ of
    Tuple "admin" "admin" -> pure (Just (User "administrator"))
    Tuple "guest" "guest" -> pure (Just (User "guest"))
    _ -> pure Nothing


-- A middleware that wraps another middleware, checking that the
-- authenticated user has role `Admin`. In this example it simply
-- checks the user name, but in a real application you would
-- likely do a database lookup or something like that.
requireAdmin
  :: forall m req res rw c.
     (Monad m, ResponseWriter rw m) =>
     (Middleware
      m
      (Conn req
            { writer :: rw StatusLineOpen | res }
            { authentication :: Authorized Admin | c })
      (Conn req
            { writer :: rw ResponseEnded | res }
            { authentication :: Authorized Admin | c }))
  -> Middleware
     m
     (Conn req
           { writer :: rw StatusLineOpen | res }
           { authentication :: User | c })
     (Conn req
           { writer :: rw ResponseEnded | res }
           { authentication :: User | c })
requireAdmin mw conn =
  case conn.components.authentication of
    User "administrator" ->
      conn
      # setAuthentication (Authorized conn.components.authentication Admin)
      # mw
      # map (setAuthentication conn.components.authentication)
    _ ->
      writeStatus (Tuple 403 "Forbidden") conn
      >>= headers []
      >>= respond "You are not authorized."


-- A handler that does not require an authenticated user, but displays the
-- name if the user _is_ authenticated.
profileHandler
  :: forall m req res rw c.
     (Monad m, ResponseWriter rw m) =>
     Middleware
     m
     (Conn { url :: String, method :: Method, headers :: StrMap String | req }
           { writer :: rw StatusLineOpen | res }
           { authentication :: Maybe User | c })
     (Conn { url :: String, method :: Method, headers :: StrMap String | req }
           { writer :: rw ResponseEnded | res }
           { authentication :: Maybe User | c })
profileHandler conn =
  htmlWithStatus
  statusOK
  (view conn.components.authentication)
  conn
  where
    view =
      case _ of
        Just (User name) -> do
          h1 [] (text "Profile")
          p [] (text ("Logged in as " <> name <> "."))
        Nothing ->
          p [] (text "You are not logged in.")


-- A handler that requires a `User` authorized as `Admin`. Note that
-- even if the actual authorization check does not happen here, we
-- cannot user this handler without doing authorization properly
-- before.
adminHandler
  :: forall m req res rw c.
     (Monad m, ResponseWriter rw m) =>
     Middleware
     m
     (Conn { url :: String, method :: Method, headers :: StrMap String | req }
           { writer :: rw StatusLineOpen | res }
           { authentication :: Authorized Admin | c })
     (Conn { url :: String, method :: Method, headers :: StrMap String | req }
           { writer :: rw ResponseEnded | res }
           { authentication :: Authorized Admin | c })
adminHandler conn =
  htmlWithStatus
  statusOK
  (view conn.components.authentication)
  conn
  where
    view (Authorized (User name) Admin) = do
      h1 [] (text "Administration")
      p [] (text ("Here be dragons, " <> name <> "."))


app :: forall e req res rw c.
       (ResponseWriter rw (Aff (buffer :: BUFFER | e))) =>
       Middleware
       (Aff (buffer :: BUFFER | e))
       (Conn { url :: String, method :: Method, headers :: StrMap String | req }
             { writer :: rw StatusLineOpen | res }
             { authentication :: Unit | c })
       (Conn { url :: String, method :: Method, headers :: StrMap String | req }
             { writer :: rw ResponseEnded | res }
             { authentication :: Maybe User | c })
app =
  -- We always check for authentication.
  BasicAuth.withAuthentication userFromBasicAuth
  >=> fallbackTo notFound (resource home <|> resource profile <|> resource admin)
    where
      notFound = htmlWithStatus
                 statusNotFound
                 (text "Not Found")

      homeView = do
        h1 [] (text "Home")
        ul [] do
          li [] (linkTo profile (text "Profile"))
          li [] (linkTo admin (text "Administration"))

      home = { path: []
             , "GET":
               handler (htmlWithStatus statusOK homeView)
             , "POST": notSupported
             }

      profile = { path: ["profile"]
                , "GET": handler profileHandler
                , "POST": notSupported
                }

      admin = { path: ["admin"]
              -- To use the admin handler, we must ensure that the user is
              -- authenticated and authorized as `Admin`.
              , "GET": handler (BasicAuth.authenticated (requireAdmin adminHandler))
              , "POST": notSupported
              }

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  let
    -- Some nice console printing when the server starts, and if a request
    -- fails (in this case when the request body is unreadable for some reason).
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
    components = { authentication: unit }

  -- Let's run it.
  in runServer defaultOptions onListening onRequestError components app
