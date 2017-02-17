module Site2 where

import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Array (find)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Data.Traversable (traverse_)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Port (Port(..))
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture, (:<|>))
import Hyper.Routing.ContentType.HTML (class EncodeHTML, HTML, linkTo)
import Hyper.Routing.Links (linksTo)
import Hyper.Routing.Method (Get)
import Hyper.Routing.Router (RoutingError(..), router)
import Hyper.Status (statusNotFound)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (div, h1, li, p, ul)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Prelude hiding (div)

data Home = Home

data AllUsers = AllUsers (Array User)

newtype User = User { id :: Int, name :: String }

type Site2 =
  Get HTML Home
  :<|> "users" :/ Get HTML AllUsers
  :<|> "users" :/ Capture "user-id" Int :> Get HTML User

site2 :: Proxy Site2
site2 = Proxy

home :: forall m. Monad m => ExceptT RoutingError m Home
home = pure Home

allUsers :: forall m. Monad m => ExceptT RoutingError m AllUsers
allUsers = AllUsers <$> getUsers

getUser :: forall m. Monad m => Int -> ExceptT RoutingError m User
getUser id' =
  find userWithId <$> getUsers >>=
  case _ of
    Just user -> pure user
    Nothing ->
      throwError (HTTPError { status: statusNotFound
                            , message: Just "User not found."
                            })
  where
    userWithId (User u) = u.id == id'

instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    case linksTo site2 of
      _ :<|> allUsers' :<|> _ ->
        p do
          text "Welcome to my site! Go check out my "
          linkTo allUsers' (text "Users")
          text "."

instance encodeHTMLAllUsers :: EncodeHTML AllUsers where
  encodeHTML (AllUsers users) =
    div do
      h1 (text "Users")
      ul (traverse_ linkToUser users)
    where
      linkToUser (User u) =
        case linksTo site2 of
          _ :<|> _ :<|> getUser' ->
            li (linkTo (getUser' u.id) (text u.name))

instance encodeHTMLUser :: EncodeHTML User where
  encodeHTML (User { name }) =
    h1 (text name)

getUsers :: forall m. Applicative m => m (Array User)
getUsers =
  pure
  [ User { id: 1, name: "John Paul Jones" }
  , User { id: 2, name: "Tal Wilkenfeld" }
  , User { id: 3, name: "John Patitucci" }
  , User { id: 4, name: "Jaco Pastorious" }
  ]

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e) Unit
main =
  let otherSiteRouter =
        router site2 (home :<|> allUsers :<|> getUser) onRoutingError

      onRoutingError status msg =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" id msg)

      onListening (Port port) =
        log ("Listening on http://localhost:" <> show port)

      onRequestError err =
        log ("Request failed: " <> show err)

  in runServer defaultOptions onListening onRequestError {} otherSiteRouter
