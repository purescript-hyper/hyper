module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Hyper.Core (Port(Port))
import Hyper.HTML.DSL (h1, html, text)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (headers)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    app = headers []
          >=> html (h1 (text "Hello, Hyper!"))
  in runServer defaultOptions onListening app
