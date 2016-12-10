module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Hyper.Core (Port(..))
import Hyper.HTML.DSL (h1, html, text)
import Hyper.Node.Server (runServer)
import Hyper.Response (headers)

main :: forall e. Eff ( console :: CONSOLE | e ) Unit
main = runServer (Port 3000) $
       headers [] >=> html (h1 (text "Hello, Hyper!"))
