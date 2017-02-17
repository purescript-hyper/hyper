module Examples.NodeStreamResponse where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Traversable (traverse_)
import Hyper.Node.Server (defaultOptions, runServer, writeString)
import Hyper.Port (Port(..))
import Hyper.Response (closeHeaders, end, send, writeStatus)
import Hyper.Status (statusOK)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

    streamMessages =
      traverse_
      (send <<< writeString UTF8 <<< flip (<>) "\n")
      ["Hello", "Streaming", "Hyper"]

    app = do
      writeStatus statusOK
      :*> closeHeaders
      :*> streamMessages
      :*> end
  in runServer defaultOptions onListening onRequestError {} app
