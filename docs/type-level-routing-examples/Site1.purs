module Site1 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Core (Port(Port), closeHeaders, writeStatus)
import Hyper.HTML (class EncodeHTML, HTML, p, text)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (contentType, respond)
import Hyper.Routing.Method (Get)
import Hyper.Routing.Router (router)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Type.Proxy (Proxy(..))

data Home = Home

type Site1 = Get HTML Home

home :: forall m. Applicative m => m Home
home = pure Home

instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    p [] [ text "Welcome to my site!" ]

mySite :: Proxy Site1
mySite = Proxy

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e) Unit
main =
  runServer defaultOptions onListening onRequestError {} siteRouter
  where
    onListening (Port port) =
      log ("Listening on http://localhost:" <> show port)

    onRequestError err =
      log ("Request failed: " <> show err)

    onRoutingError status msg =
      writeStatus status
      >=> contentType textHTML
      >=> closeHeaders
      >=> respond (maybe "" id msg)

    siteRouter = router mySite home onRoutingError
