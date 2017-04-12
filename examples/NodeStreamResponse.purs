-- This example shows how you can stream responses asynchronously.
--
-- Test it out like so:
-- $ curl -N http://localhost:3000
module Examples.NodeStreamResponse where

import Prelude
import Control.Monad.Aff as Aff
import Control.IxMonad ((:*>))
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Int (toNumber)
import Data.Newtype (wrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Hyper.Middleware (Middleware, lift')
import Hyper.Node.Server (defaultOptions, runServer, writeString)
import Hyper.Response (closeHeaders, end, send, writeStatus)
import Hyper.Status (statusOK)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP)

delay :: forall m e c. MonadAff e m => Int -> Middleware m c c Unit
delay n = lift' (liftAff (Aff.delay (wrap <<< toNumber $ n) *> pure unit))

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let
    -- These messages are streamed one at a time, regardless of their individual
    -- delays, i.e. they're sequenced.
    streamMessages =
      traverse_
      (\(Tuple ms s) -> delay ms :*> send (writeString UTF8 s))
      [ Tuple 2000 "Hello\n"
      , Tuple 1000 "Streaming\n"
      , Tuple 500 "Hyper\n"
      ]

    app = do
      writeStatus statusOK
      :*> closeHeaders
      :*> streamMessages
      :*> end
  in runServer defaultOptions {} app
