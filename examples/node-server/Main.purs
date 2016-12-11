module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Hyper.HTML.DSL (h1, html, text)
import Hyper.Node.Server (defaultListenOptions, runServer)
import Hyper.Response (headers)
import Node.HTTP (HTTP)

main :: forall e. Eff ( http ∷ HTTP | e ) Unit
main = runServer defaultListenOptions $
       headers [] >=> html (h1 (text "Hello, Hyper!"))
