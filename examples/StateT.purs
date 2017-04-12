module Examples.StateT where

import Prelude
import Control.IxMonad (ibind, (:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.State.Trans (StateT)
import Data.String (joinWith)
import Hyper.Middleware (lift')
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)
import Node.HTTP (HTTP)


runAppM ∷ ∀ e a. StateT (Array String) (Aff e) a → Aff e a
runAppM = flip evalStateT []


main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let
      -- Our application just appends to the state in between
      -- some operations, then responds with the built up state...
      app = do
        _ <- lift' (modify (flip append ["I"]))
          :*> writeStatus statusOK
          :*> lift' (modify (flip append ["have"]))
          :*> closeHeaders
          :*> lift' (modify (flip append ["state."]))

        msgs ← lift' get
        respond (joinWith " " msgs)

        where bind = ibind

  in runServer' defaultOptionsWithLogging {} runAppM app
