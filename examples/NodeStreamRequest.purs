-- This example shows how you can stream request body data. It
-- logs the size of each chunk it receives from the POST body.
--
-- Test it out by first running the server,
--
--     $ pulp run -I examples -m Examples.NodeStreamRequest
--
-- and then, POST a large file with something like this command:
--
--     $ curl -X POST --data-binary @/your/large/file localhost:3000
--
module Examples.NodeStreamRequest where

import Prelude
import Node.Buffer as Buffer
import Node.Stream as Stream
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed ((:>>=))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (catchException, message)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (getRequestData, streamBody)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusMethodNotAllowed, statusOK)

logRequestBodyChunks
  :: forall m
   . MonadEffect m
  => Stream.Readable ()
  -> m Unit
logRequestBodyChunks body =
  Stream.onData body (Buffer.size >=> (log <<< ("Got chunk of size: " <> _) <<< show))
  # catchException (log <<< ("Error: " <> _) <<< message)
  # liftEffect

main :: Effect Unit
main =
  let
    app =
      getRequestData :>>=
      case _ of

        -- Only handle POST requests:
        { method: Left POST } -> Ix.do
            body <- streamBody
            logRequestBodyChunks body
            writeStatus statusOK
            closeHeaders
            respond "OK"

        -- Non-POST requests are not allowed:
        { method } -> Ix.do
          writeStatus statusMethodNotAllowed
          closeHeaders
          respond ("Method not allowed: " <> either show show method)

  in runServer defaultOptionsWithLogging {} app
