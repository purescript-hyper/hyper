-- This is a bit bigger example, featuring _authentication_ and
-- _authorization_, illustrating the parts that can be custom
-- to your application, and how you can leverage the type system
-- to make sure authorization is properly checked.
--
-- It _does not_ feature type-safe routing, to keep the example
-- focused on auth.
module Main where

import Prelude
import Hyper.Node.BasicAuth as BasicAuth
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.MediaType.Common (textHTML)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Authorization (authorized)
import Hyper.Core (writeStatus, StatusLineOpen, class ResponseWriter, ResponseEnded, Conn, Middleware, closeHeaders, Port(Port))
import Hyper.HTML (HTML, a, asString, element_, h1, li, p, text, ul)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (class Response, respond, contentType)
import Hyper.Status (Status, statusNotFound, statusOK)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)


-- Helper for responding with HTML.
htmlWithStatus
  :: forall m req res rw b c.
     (Monad m, ResponseWriter rw m b, Response b m String) =>
     Status
  -> HTML
  -> Middleware
     m
     (Conn req { writer :: rw StatusLineOpen | res } c)
     (Conn req { writer :: rw ResponseEnded | res } c)
htmlWithStatus status x =
  writeStatus status
  >=> contentType textHTML
  >=> closeHeaders
  >=> respond (asString x)


-- Users have user names.
type Name = String
data User = User Name


-- In this example there is a single authorization role that users can have.
--
-- Given that roles are static, you can represent each role with a distinct
-- type (instead of having a single type with multiple constructors) to get
-- compile-time errors when checks are missing.
data Admin = Admin


-- A handler that does not require an authenticated user, but displays the
-- name if the user _is_ authenticated.
profileHandler
  :: forall m req res rw b c.
     (Monad m, ResponseWriter rw m b, Response b m String) =>
     Middleware
     m
     (Conn req { writer :: rw StatusLineOpen | res } { authentication :: Maybe User | c })
     (Conn req { writer :: rw ResponseEnded | res } { authentication :: Maybe User | c })
profileHandler conn =
  htmlWithStatus
  statusOK
  (view conn.components.authentication)
  conn
  where
    view =
      case _ of
        Just (User name) ->
          element_ "section" [ h1 [] [ text "Profile" ]
                             , p [] [ text ("Logged in as " <> name <> ".") ]
                             ]
        Nothing ->
          p [] [text "You are not logged in."]


-- A handler that requires a user authorized as `Admin`. Note that
-- even though the actual authentication and authorization checks are
-- not made here, we can be confident they have been made somewhere
-- before in the middleware chain. This allows you to safely and
-- confidently refactor and evolve the application, without having
-- to scatter authentication and authorization checks all over the
-- place . You simply mark the requirement in the type signature,
-- as seen below.
adminHandler
  :: forall m req res rw b c.
     (Monad m, ResponseWriter rw m b, Response b m String) =>
     Middleware
     m
     (Conn req { writer :: rw StatusLineOpen | res } { authorization :: Admin, authentication :: User | c })
     (Conn req { writer :: rw ResponseEnded | res } { authorization :: Admin, authentication :: User | c })
adminHandler conn =
  htmlWithStatus
  statusOK
  (view conn.components.authentication)
  conn
  where
    view (User name) =
      element_ "section" [ h1 [] [ text "Administration" ]
                         , p [] [ text ("Here be dragons, " <> name <> ".") ]
                         ]


-- This could be a function checking the username/password in a database
-- in your application.
userFromBasicAuth :: forall m e. MonadAff e m => Tuple String String -> m (Maybe User)
userFromBasicAuth =
  case _ of
    Tuple "admin" "admin" -> pure (Just (User "admin"))
    Tuple "guest" "guest" -> pure (Just (User "guest"))
    _ -> pure Nothing


-- This could be a function checking a database, or some session store, if the
-- authenticated user has role `Admin`.
getAdminRole :: forall m req res c.
                Monad m =>
                Conn
                req
                res
                { authentication :: User , authorization :: Unit | c }
             -> m (Maybe Admin)
getAdminRole conn =
  case conn.components.authentication of
    User "admin" -> pure (Just Admin)
    _ -> pure Nothing


app :: forall m e req res rw b c.
       (MonadAff (buffer :: BUFFER | e) m, ResponseWriter rw m b, Response b m String) =>
       Middleware
       m
       (Conn { url :: String, method :: Either Method CustomMethod, headers :: StrMap String | req }
             { writer :: rw StatusLineOpen | res }
             { authentication :: Unit
             , authorization :: Unit
             | c
             })
       (Conn { url :: String, method :: Either Method CustomMethod, headers :: StrMap String | req }
             { writer :: rw ResponseEnded | res }
             { authentication :: Maybe User
             , authorization :: Unit
             | c
             })
app = BasicAuth.withAuthentication userFromBasicAuth >=> router
    where
      notFound = htmlWithStatus
                 statusNotFound
                 (text "Not Found")


      homeView =
        element_ "section" [ h1 [] [text "Home"]
                           , ul [] [ li [] [ a [ Tuple "href" "/profile" ] [ text "Profile" ] ]
                                   , li [] [ a [ Tuple "href" "/admin" ] [ text "Administration" ] ]
                                   ]
                           ]

      router conn =
        case Tuple conn.request.method conn.request.url of
          Tuple (Left GET) "/" ->
            htmlWithStatus statusOK homeView conn
          Tuple (Left GET) "/profile" ->
            profileHandler conn
          Tuple (Left GET) "/admin" ->
              -- To use the admin handler, we must ensure that the user is
              -- authenticated and authorized as `Admin`.
            BasicAuth.authenticated
            "Authorization Example"
            (authorized getAdminRole adminHandler)
            conn
          _ ->
            notFound conn

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
    components = { authentication: unit
                 , authorization: unit
                 }
  in runServer defaultOptions onListening onRequestError components app
