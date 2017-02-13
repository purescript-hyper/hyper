module Main where

import Prelude
import Control.IxMonad (ibind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Port (Port(..))
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP, buffer :: BUFFER | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
    app = do
      writeStatus statusOK
      closeHeaders
      respond "Hello, Hyper!"
      where bind = ibind
  in runServer defaultOptions onListening onRequestError {} app
