module Examples.Cookies where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Effect (Effect)
import Hyper.Cookies (cookies)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

main :: Effect Unit
main =
  let app = Ix.do
        cookies
        writeStatus statusOK
        closeHeaders
        respond "Hello, Hyper!"
  in runServer defaultOptionsWithLogging { cookies: unit } app
