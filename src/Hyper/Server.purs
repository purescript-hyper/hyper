module Hyper.Server where

import Prelude
import Control.Monad.Cont.Trans (ContT(..), lift, runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Hyper.Core (class ResponseWriter, Conn, HeadersClosed(..), HeadersOpen(..), Middleware, ResponseEnded(..))

newtype Port = Port Int

derive instance newtypePort :: Newtype Port _

foreign import data IncomingMessage :: *
foreign import data ServerResponse :: *

foreign import _end :: forall e. ServerResponse -> Eff e Unit

instance responseWriterServerResponse :: MonadEff e m => ResponseWriter ServerResponse m where
  writeHeader _ (Tuple name value) conn = pure conn
  closeHeaders _ { request, response, components } =
    pure { request: request
         , response: (response { state = HeadersClosed })
         , components: components
         }
  endResponse writer { request, response, components } = do
    liftEff (_end writer)
    pure { request: request
         , response: (response { state = ResponseEnded })
         , components: components
         }

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
             -> Middleware 
                m 
                (Conn { host :: String } { state :: HeadersOpen, writer :: ServerResponse } {}) 
                (Conn req { state :: ResponseEnded | res } c)
             -> m Unit
runServer port middleware = runContT action (const $ pure unit)
  where
    action = do
      serverConn <- serveHttp port
      let conn = { request: { host: "foo" }
                 , response: { state: HeadersOpen, writer: serverConn.response }
                 , components: {}
                 }
      conn' <- lift (middleware conn)
      liftEff (log $ "Got a request to: " <> conn.request.host)
