module Examples.Cookies where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Hyper.Node.Cookie (cookies)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let app = cookies
            :*> writeStatus statusOK
            :*> closeHeaders
            :*> respond "Hello, Hyper!"
  in runServer defaultOptionsWithLogging { cookies: unit } app
