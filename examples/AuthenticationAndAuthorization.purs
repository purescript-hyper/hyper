-- This is a bit bigger example, featuring _authentication_ and
-- _authorization_, illustrating the parts that can be custom
-- to your application, and how you can leverage the type system
-- to make sure authorization is properly checked.
--
-- It _does not_ feature type-safe routing, to keep the example
-- focused on auth.
module Examples.AuthenticationAndAuthorization where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed ((:>>=))
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(Nothing, Just))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(Tuple))
import Hyper.Authorization (authorized)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Middleware.Class (getConn)
import Hyper.Node.BasicAuth as BasicAuth
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (Status, statusNotFound, statusOK)
import Text.Smolder.HTML (a, h1, li, p, section, ul)
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.String (render)


-- Helper for responding with HTML.
htmlWithStatus
  :: forall m req res b c
  .  Monad m
  => Response res m b
  => ResponseWritable b m String
  => Status
  -> Markup Unit
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
htmlWithStatus status x = Ix.do
  writeStatus status
  contentType textHTML
  closeHeaders
  respond (render x)


-- Users have user names.
newtype User = User { name :: String }


-- In this example there is a single authorization role that users can have.
--
-- Given that roles are static, you can represent each role with a distinct
-- type (instead of having a single type with multiple constructors) to get
-- compile-time errors when checks are missing.
data Admin = Admin


-- A handler that does not require an authenticated user, but displays the
-- name if the user _is_ authenticated.
profileHandler
  :: forall m req res b c
  .  Monad m
  => Response res m b
  => ResponseWritable b m String
  => Middleware
     m
     (Conn req (res StatusLineOpen) { authentication :: Maybe User | c })
     (Conn req (res ResponseEnded) { authentication :: Maybe User | c })
     Unit
profileHandler = Ix.do
  conn <- getConn
  htmlWithStatus
    statusOK
    (view conn.components.authentication)
  where
    view =
      case _ of
        Just (User { name }) ->
          section do
            h1 (text "Profile")
            p (text ("Logged in as " <> name <> "."))
        Nothing ->
          p (text "You are not logged in.")


-- A handler that requires a user authorized as `Admin`. Note that
-- even though the actual authentication and authorization checks are
-- not made here, we can be confident they have been made somewhere
-- before in the middleware chain. This allows you to safely and
-- confidently refactor and evolve the application, without having
-- to scatter authentication and authorization checks all over the
-- place . You simply mark the requirement in the type signature,
-- as seen below.
adminHandler
  :: forall m req res b c
  .  Monad m
  => Response res m b
  => ResponseWritable b m String
  => Middleware
     m
     (Conn req (res StatusLineOpen) { authorization :: Admin, authentication :: User | c })
     (Conn req (res ResponseEnded) { authorization :: Admin, authentication :: User | c })
     Unit
adminHandler =
  getConn :>>= \conn →
  htmlWithStatus
  statusOK
  (view conn.components.authentication)
  where
    view (User { name }) =
      section do
        h1 (text "Administration")
        p (text ("Here be dragons, " <> name <> "."))


-- This could be a function checking the username/password in a database
-- in your application.
userFromBasicAuth
  :: forall m. MonadAff m =>
     Tuple String String
  -> m (Maybe User)
userFromBasicAuth =
  case _ of
    Tuple "admin" "admin" -> pure (Just (User { name: "admin" }))
    Tuple "guest" "guest" -> pure (Just (User { name: "guest" }))
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
    User { name: "admin" } -> pure (Just Admin)
    _ -> pure Nothing


app :: forall m req res b c
    .  MonadAff m
    => Request req m
    => Response res m b
    => ResponseWritable b m String
    => Middleware
       m
       (Conn req
             (res StatusLineOpen)
             { authentication :: Unit
             , authorization :: Unit
             | c
             })
       (Conn req
             (res ResponseEnded)
             { authentication :: Maybe User
             , authorization :: Unit
             | c
             })
       Unit
app = BasicAuth.withAuthentication userFromBasicAuth :>>= \_ → router
    where
      notFound = htmlWithStatus
                 statusNotFound
                 (text "Not Found")

      homeView =
        section do
          h1 (text "Home")
          ul do
            li (a ! A.href "/profile" $ text "Profile")
            li (a ! A.href "/admin" $ text "Administration")

      router = Ix.do
        { method, url } <- getRequestData
        case method, url of
          Left GET, "/" ->
            htmlWithStatus statusOK homeView
          Left GET, "/profile" ->
            profileHandler
          Left GET, "/admin" ->
              -- To use the admin handler, we must ensure that the user is
              -- authenticated and authorized as `Admin`.
            BasicAuth.authenticated
            "Authorization Example"
            (authorized getAdminRole adminHandler)
          _, _ ->
            notFound

main :: Effect Unit
main =
  let
    components = { authentication: unit
                 , authorization: unit
                 }
  in runServer defaultOptionsWithLogging components app
