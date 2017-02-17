module Examples.HelloHyper where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Port (Port(..))
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
    app = do
      writeStatus statusOK
      :*> closeHeaders
      :*> respond "Hello, Hyper!"
  in runServer defaultOptions onListening onRequestError {} app
