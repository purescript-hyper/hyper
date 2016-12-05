module Hyper.Server where

import Prelude
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)

handler :: forall e m req res c req' res' c'.
           MonadEff (console :: CONSOLE | e) m =>
           Middleware m (Conn req res c) (Conn req' { body :: String | res' } c')
           -> Conn req res c
           -> m Unit
handler mw conn = do
  conn' <- mw conn
  liftEff (log conn'.response.body)

serve :: forall m e.
         MonadEff (console :: CONSOLE | e) m => 
         ContT Unit m (Conn {} {} {})
serve = ContT \h -> do
  let conn = { request: {}
             , response: {}
             , components: {}
             }
  liftEff (log "Got a request!")
  h conn

runServer :: forall e.
             (Conn {} {} {} -> Eff (console :: CONSOLE | e) Unit)
            -> Eff (console :: CONSOLE | e) Unit
runServer = runContT serve
