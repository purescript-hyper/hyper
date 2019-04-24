module Examples.QualifiedDo where

import Prelude
import Effect (Effect)
import Hyper.Middleware as Middleware
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

main :: Effect Unit
main = runServer defaultOptionsWithLogging {} Middleware.do
  writeStatus statusOK
  closeHeaders
  respond "Hello, Hyper!"
