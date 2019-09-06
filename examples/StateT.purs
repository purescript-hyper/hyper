module Examples.StateT where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Effect.Aff (Aff)
import Effect (Effect)
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.State.Trans (StateT)
import Data.String (joinWith)
import Hyper.Middleware (lift')
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)


runAppM ∷ ∀ a. StateT (Array String) Aff a → Aff a
runAppM = flip evalStateT []


main :: Effect Unit
main =
  let
      -- Our application just appends to the state in between
      -- some operations, then responds with the built up state...
      app = Ix.do
        void $ lift' (modify (flip append ["I"]))
        writeStatus statusOK
        void $ lift' (modify (flip append ["have"]))
        closeHeaders
        void $ lift' (modify (flip append ["state."]))

        msgs ← lift' get
        respond (joinWith " " msgs)

  in runServer' defaultOptionsWithLogging {} runAppM app
