module Examples.HelloHyper where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Effect (Effect)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

main :: Effect Unit
main =
  let app = Ix.do
        writeStatus statusOK
        closeHeaders
        respond "Hello, Hyper!"
  in runServer defaultOptionsWithLogging {} app
