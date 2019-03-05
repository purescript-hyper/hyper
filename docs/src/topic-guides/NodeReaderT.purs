module NodeReaderT where

import Prelude
import Control.Monad.Indexed ((:>>=), (:*>))
import Effect.Aff (Aff)
import Effect (Effect)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Hyper.Middleware (lift')
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

-- start snippet main
type MyConfig = { thingToSay :: String }

runAppM
  :: forall a.
    ReaderT MyConfig Aff a
  -> Aff a
runAppM = flip runReaderT { thingToSay: "Hello, ReaderT!" }

main :: Effect Unit
main =
  let app =
        lift' ask :>>= \config ->
          writeStatus statusOK
          :*> closeHeaders
          :*> respond config.thingToSay
  in runServer' defaultOptionsWithLogging {} runAppM app
-- end snippet main
