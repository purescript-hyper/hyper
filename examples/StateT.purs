module Examples.StateT where

import Prelude
import Control.IxMonad (ibind)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.State.Trans (StateT)
import Data.String (joinWith)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Node.Server (HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Response (ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)
import Node.HTTP (HTTP)

app
  ∷ ∀ m e req res c.
    ( Monad m
    , MonadAff (http ∷ HTTP | e) m
    ) ⇒
    Middleware
    (StateT (Array String) m)
    (Conn req { writer :: HttpResponse StatusLineOpen | res } c)
    (Conn req { writer :: HttpResponse ResponseEnded | res } c)
    Unit
app = do
  -- Append to the state here and there...
  lift' (modify (flip append ["I"]))
  writeStatus statusOK
  lift' (modify (flip append ["have"]))
  closeHeaders
  lift' (modify (flip append ["state."]))

  msgs ← lift' get
  respond (joinWith " " msgs)

  where bind = ibind


runAppM ∷ ∀ e a. StateT (Array String) (Aff e) a → Aff e a
runAppM = flip evalStateT []


main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let options = defaultOptionsWithLogging { runM = runAppM }
  in runServer options {} app
