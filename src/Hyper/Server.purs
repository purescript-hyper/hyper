module Hyper.Server where

import Prelude
import Control.Monad.Cont.Trans (ContT(..), lift, runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Newtype (class Newtype)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)

newtype Port = Port Int

derive instance newtypePort :: Newtype Port _

foreign import data IncomingMessage :: *
foreign import data ServerResponse :: *

foreign import _end :: forall e. ServerResponse -> String -> Eff e Unit

foreign import _serveHttp :: forall m e.
                             MonadEff e m =>
                             Int
                             -> ({ request :: IncomingMessage, response :: ServerResponse } -> m Unit)
                             -> m Unit

serveHttp :: forall m e.
             MonadEff e m => 
             Port
             -> ContT Unit m { request :: IncomingMessage, response :: ServerResponse }
serveHttp (Port port) = ContT (_serveHttp port)

runServer :: forall m e req res c.
             MonadEff (console :: CONSOLE | e) m =>
             Port
             -> Middleware m (Conn { host :: String } {} {}) (Conn req { body :: String | res } c)
             -> m Unit
runServer port middleware = runContT action (const $ pure unit)
  where
    action = do
      serverConn <- serveHttp port
      let conn = { request: { host: "foo" }
                 , response: {}
                 , components: {}
                 }
      conn' <- lift (middleware conn)
      liftEff do
        log $ "Got a request to: " <> conn.request.host
        _end serverConn.response conn'.response.body
  
