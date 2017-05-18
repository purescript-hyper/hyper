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
import Control.IxMonad (ibind, (:>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, message)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (getRequestData, streamBody)
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusMethodNotAllowed, statusOK)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)

type ExampleEffects e = (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e)

logRequestBodyChunks
  :: forall m e
   . MonadEff (ExampleEffects e) m
  => Stream.Readable () (ExampleEffects (exception :: EXCEPTION | e))
  -> m Unit
logRequestBodyChunks body =
  Stream.onData body (Buffer.size >=> (log <<< ("Got chunk of size: " <> _) <<< show))
  # catchException (log <<< ("Error: " <> _) <<< message)
  # liftEff

main :: forall e. Eff (ExampleEffects e) Unit
main =
  let
    app =
      getRequestData :>>=
      case _ of

        -- Only handle POST requests:
        { method: Left POST } -> do
            body <- streamBody
            logRequestBodyChunks body
            writeStatus statusOK
            closeHeaders
            respond "OK"

        -- Non-POST requests are not allowed:
        { method } -> do
          writeStatus statusMethodNotAllowed
          closeHeaders
          respond ("Method not allowed: " <> either show show method)

        where
            bind = ibind
            discard = ibind
  in runServer defaultOptionsWithLogging {} app
