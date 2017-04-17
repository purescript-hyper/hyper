module MultiMethodExample where

import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Routing (type (:<|>), Resource, (:<|>))
import Hyper.Routing.ContentType.HTML (class EncodeHTML, HTML)
import Hyper.Routing.Method (Get, Delete)
import Hyper.Routing.Router (RoutingError(..), router)
import Hyper.Status (statusBadRequest)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Prelude hiding (div)

newtype User = User { name :: String }

-- start snippet routing-type
type MultiMethodExample =
  Resource (Get User :<|> Delete User) HTML
-- end snippet routing-type

site :: Proxy MultiMethodExample
site = Proxy

getUsers :: forall m. Monad m => ExceptT RoutingError m User
getUsers =
  pure (User { name: "An existing user." })

deleteUser :: forall m. Monad m => ExceptT RoutingError m User
deleteUser =
  throwError (HTTPError { status: statusBadRequest
                        , message: Just "Not doing that, no..."
                        })

instance encodeHTMLUser :: EncodeHTML User where
  encodeHTML (User { name }) =
    h1 (text $ "User: " <> name)

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e) Unit
main =
  let site3Router =
        -- start snippet router
        router site (getUsers :<|> deleteUser) onRoutingError
        -- end snippet router

      onRoutingError status msg =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" id msg)

  in runServer defaultOptions {} site3Router
