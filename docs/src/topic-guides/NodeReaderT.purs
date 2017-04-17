module NodeReaderT where

import Prelude
import Control.IxMonad ((:>>=), (:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Hyper.Middleware (lift')
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)
import Node.HTTP (HTTP)

-- start snippet main
type MyConfig = { thingToSay :: String }

runAppM
  :: forall e a.
    ReaderT MyConfig (Aff e) a
  -> Aff e a
runAppM = flip runReaderT { thingToSay: "Hello, ReaderT!" }

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let app =
        lift' ask :>>= \config ->
          writeStatus statusOK
          :*> closeHeaders
          :*> respond config.thingToSay
  in runServer' defaultOptionsWithLogging {} runAppM app
-- end snippet main
