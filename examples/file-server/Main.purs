module Main where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple))
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Port (Port(..))
import Hyper.Response (headers, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP, fs :: FS, buffer :: BUFFER | e) Unit
main =
  let
    onListening (Port port) = log ("Serving files at http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
    notFound =
      writeStatus statusNotFound
      :*> headers []
      :*> respond (Tuple "<h1>Not Found</h1>" UTF8)
    app = fileServer "examples/file-server" notFound
  in runServer defaultOptions onListening onRequestError {} app
