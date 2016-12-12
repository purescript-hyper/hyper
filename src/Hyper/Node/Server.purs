module Hyper.Node.Server where

import Node.HTTP
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Function ((<<<))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Hyper.Core (class ResponseWriter, Conn, HeadersClosed(..), HeadersOpen(..), Middleware, Port(..), ResponseEnded(..))
import Node.Encoding (Encoding(..))
import Node.Stream (end, writeString)

data HttpResponse state = HttpResponse state Response

withState ∷ ∀ req res c a b.
            b 
          → Conn req { writer ∷ HttpResponse a | res } c
          → Conn req { writer ∷ HttpResponse b | res } c
withState s conn = 
  case conn.response.writer of
       HttpResponse _ r → conn { response = (conn.response { writer = HttpResponse s r }) }

  

instance responseWriterHttpResponse :: MonadEff (http ∷ HTTP | e) m => ResponseWriter HttpResponse m where
  writeHeader (Tuple name value) conn =
    pure conn

  closeHeaders = pure <<< withState HeadersClosed

  send s conn = 
    case conn.response.writer of
      HttpResponse _ r → do
        liftEff (writeString (responseAsStream r) UTF8 s (pure unit))
        pure conn

  end conn = do
    case conn.response.writer of
      HttpResponse _ r → do
        liftEff (end (responseAsStream r) (pure unit))
        pure (withState ResponseEnded conn)

type ServerOptions = { hostname ∷ String
                     , port ∷ Port
                     }

defaultOptions ∷ ServerOptions
defaultOptions = { hostname: "0.0.0.0"
                 , port: Port 3000
                 }

runServer :: forall e req res c.
             ServerOptions
             -> (Port -> Eff (http ∷ HTTP | e) Unit)
             -> Middleware
                (Eff (http ∷ HTTP | e))
                (Conn { host :: String } { writer :: HttpResponse HeadersOpen } {})
                (Conn req { writer :: HttpResponse ResponseEnded | res } c)
             -> Eff (http ∷ HTTP | e) Unit
runServer options onListening middleware = do
  server <- createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: "0.0.0.0"
                      , backlog: Nothing
                      }
  listen server listenOptions (onListening options.port)
  where
    onRequest ∷ Request → Response → Eff (http ∷ HTTP | e) Unit
    onRequest request response = do
      let conn = { request: { host: "foo" }
                 , response: { writer: HttpResponse HeadersOpen response }
                 , components: {}
                 }
      conn' <- middleware conn
      pure unit
