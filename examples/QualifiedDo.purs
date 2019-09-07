module Examples.QualifiedDo where

import Prelude
import Effect (Effect)
import Control.Monad.Indexed.Qualified as Ix
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

main :: Effect Unit
main = runServer defaultOptionsWithLogging {} Ix.do
  writeStatus statusOK
  closeHeaders
  respond "Hello, Hyper!"
