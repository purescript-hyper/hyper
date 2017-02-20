module Site1 where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Port (Port(Port))
import Hyper.Response (contentType, respond, closeHeaders, writeStatus)
import Hyper.Routing.ContentType.HTML (class EncodeHTML, HTML)
import Hyper.Routing.Method (Get)
import Hyper.Routing.Router (router)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (p)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))

-- start snippet routing-type
data Home = Home

type Site1 = Get HTML Home
-- end snippet routing-type

-- start snippet handler
home :: forall m. Applicative m => m Home
home = pure Home
-- end snippet handler

-- start snippet encoding
instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    p (text "Welcome to my site!")
-- end snippet encoding

-- start snippet proxy
site1 :: Proxy Site1
site1 = Proxy
-- end snippet proxy

-- start snippet main
main :: forall e. Eff (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e) Unit
main =
  runServer defaultOptions onListening onRequestError {} siteRouter
  where
    onListening (Port port) =
      log ("Listening on http://localhost:" <> show port)

    onRequestError err =
      log ("Request failed: " <> show err)
-- end snippet main

-- start snippet router
    onRoutingError status msg =
      writeStatus status
      :*> contentType textHTML
      :*> closeHeaders
      :*> respond (maybe "" id msg)

    siteRouter = router site1 home onRoutingError
-- end snippet router
