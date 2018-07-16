module Examples.HelloHyper where

import Prelude
import Control.IxMonad ((:*>))
import Effect (Effect)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

main :: Effect Unit
main =
  let app = writeStatus statusOK
            :*> closeHeaders
            :*> respond "Hello, Hyper!"
  in runServer defaultOptionsWithLogging {} app
