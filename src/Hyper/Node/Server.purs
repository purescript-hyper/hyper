module Hyper.Node.Server where

import Node.HTTP
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Hyper.Core (class ResponseWriter, Conn, HeadersClosed(..), HeadersOpen(..), Middleware, ResponseEnded(..))
import Node.Encoding (Encoding(..))
import Node.Stream (end, writeString)

data HttpResponse = HttpResponse Response

instance responseWriterHttpResponse :: MonadEff (http ∷ HTTP | e) m => ResponseWriter HttpResponse m where
  writeHeader _ (Tuple name value) conn =
    pure conn

  closeHeaders _ conn =
    pure conn { response = (conn.response { state = HeadersClosed }) }

  send (HttpResponse writer) s conn = do
    liftEff (writeString (responseAsStream writer) UTF8 s (pure unit))
    pure conn

  end (HttpResponse writer) conn = do
    liftEff (end (responseAsStream writer) (pure unit))
    pure conn { response = (conn.response { state = ResponseEnded }) }

defaultListenOptions ∷ ListenOptions
defaultListenOptions = { hostname: "0.0.0.0", port: 3000, backlog: Nothing }

runServer :: forall e req res c.
             ListenOptions
             -> Middleware
                (Eff (http ∷ HTTP | e))
                (Conn { host :: String } { state :: HeadersOpen, writer :: HttpResponse } {})
                (Conn req { state :: ResponseEnded | res } c)
             -> Eff (http ∷ HTTP | e) Unit
runServer listenOptions middleware = do
  server <- createServer onRequest
  listen server listenOptions onListening
  where
    onRequest ∷ Request → Response → Eff (http ∷ HTTP | e) Unit
    onRequest request response = do
      let conn = { request: { host: "foo" }
                 , response: { state: HeadersOpen, writer: HttpResponse response }
                 , components: {}
                 }
      conn' <- middleware conn
      pure unit
    onListening = pure unit
