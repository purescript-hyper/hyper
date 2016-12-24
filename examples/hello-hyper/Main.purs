module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (closeHeaders, writeStatus, Port(Port))
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (respond)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
    app = writeStatus (Tuple 200 "OK")
          >=> closeHeaders
          >=> (respond "Hello, Hyper!")
  in runServer defaultOptions onListening onRequestError {} app
