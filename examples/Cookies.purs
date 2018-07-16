module Examples.Cookies where

import Prelude
import Control.IxMonad ((:*>))
import Effect (Effect)
import Hyper.Cookies (cookies)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

main :: Effect Unit
main =
  let app = cookies
            :*> writeStatus statusOK
            :*> closeHeaders
            :*> respond "Hello, Hyper!"
  in runServer defaultOptionsWithLogging { cookies: unit } app
