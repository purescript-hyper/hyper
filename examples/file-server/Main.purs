module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Hyper.Core (fallbackTo, writeStatus, statusNotFound, try, Port(Port))
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (respond, headers)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP, fs :: FS, buffer :: BUFFER | e) Unit
main =
  let
    onListening (Port port) = log ("Serving files at http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
    notFound =
      writeStatus statusNotFound
      >=> headers []
      >=> respond "<h1>Not Found</h1>"
    app = try (fileServer "examples/file-server")
          # fallbackTo notFound
  in runServer defaultOptions onListening onRequestError {} app
