module Examples.FileServer where

import Prelude

import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Tuple (Tuple(Tuple))
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (headers, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP, fs :: FS, buffer :: BUFFER | e) Unit
main =
  let
    notFound =
      writeStatus statusNotFound
      :*> headers []
      :*> respond (Tuple "<h1>Not Found</h1>" UTF8)
    app = fileServer "examples/FileServer" notFound
  in runServer defaultOptionsWithLogging {} app
