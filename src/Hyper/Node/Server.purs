module Hyper.Node.Server where

import Node.HTTP
import Data.StrMap as StrMap
import Hyper.Method as Method
import Node.Stream as Stream
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad (void, (>>=))
import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Aff.AVar (putVar, takeVar, modifyVar, makeVar', AVAR, makeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, Error)
import Data.Function (($), (<<<))
import Data.Int (fromString)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup ((<>))
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Hyper.Core (StatusLineOpen(StatusLineOpen), class ResponseWriter, Conn, BodyOpen(..), HeadersOpen(..), Middleware, Port(..), ResponseEnded(..))
import Hyper.Method (Method)
import Node.Encoding (Encoding(..))


newtype RequestBody = RequestBody Request

derive instance newtypeRequestBody :: Newtype RequestBody _

readBody :: forall e. RequestBody -> Aff (http :: HTTP, err :: EXCEPTION, avar :: AVAR | e) String
readBody body = do
  let stream = requestAsStream (unwrap body)
  completeBody <- makeVar
  chunks <- makeVar' ""
  liftEff do
    Stream.onDataString stream UTF8 \chunk -> void do
      launchAff (modifyVar (_ <> chunk) chunks)
    Stream.onEnd stream $ void (launchAff (takeVar chunks >>= putVar completeBody))
  takeVar completeBody

readBodyAsString ∷ ∀ e req res c.
                   Middleware
                   (Aff (http ∷ HTTP, err :: EXCEPTION, avar :: AVAR | e))
                   (Conn { body ∷ RequestBody
                         , contentLength :: Maybe Int
                         | req
                         } res c)
                   (Conn {body ∷ String, contentLength :: Maybe Int | req} res c)
readBodyAsString conn = do
  s <- readBody conn.request.body
  pure (conn { request = (conn.request { body = s }) })

data HttpResponse state = HttpResponse state Response


withState ∷ ∀ req res c a b.
            b
          → Conn req { writer ∷ HttpResponse a | res } c
          → Conn req { writer ∷ HttpResponse b | res } c
withState s conn =
  case conn.response.writer of
       HttpResponse _ r → conn { response = (conn.response { writer = HttpResponse s r }) }


instance responseWriterHttpResponse :: MonadEff (http ∷ HTTP | e) m => ResponseWriter HttpResponse m where
  writeStatus (Tuple code reason) conn =
    case conn.response.writer of
      HttpResponse _ r → do
        liftEff do
          setStatusCode r code
          setStatusMessage r reason
        pure (withState HeadersOpen conn)

  writeHeader (Tuple name value) conn =
    case conn.response.writer of
      HttpResponse _ r → do
        liftEff (setHeader r name value)
        pure conn

  closeHeaders = pure <<< withState BodyOpen

  send s conn =
    case conn.response.writer of
      HttpResponse _ r → do
        liftEff (Stream.writeString (responseAsStream r) UTF8 s (pure unit))
        pure conn

  end conn = do
    case conn.response.writer of
      HttpResponse _ r → do
        liftEff (Stream.end (responseAsStream r) (pure unit))
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
             -> (Error -> Eff (http ∷ HTTP | e) Unit)
             -> Middleware
                (Aff (http :: HTTP | e))
                (Conn { url :: String
                      , body :: RequestBody
                      , contentLength :: Maybe Int
                      , headers :: StrMap String
                      , method :: Method
                      }
                      { writer :: HttpResponse StatusLineOpen }
                      {})
                (Conn req { writer :: HttpResponse ResponseEnded | res } c)
             -> Eff (http :: HTTP | e) Unit
runServer options onListening onRequestError middleware = do
  server <- createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: "0.0.0.0"
                      , backlog: Nothing
                      }
  listen server listenOptions (onListening options.port)
  where
    parseContentLength headers = StrMap.lookup "content-length" headers
    onRequest ∷ Request → Response → Eff (http :: HTTP | e) Unit
    onRequest request response =
      let headers = requestHeaders request
          conn = { request: { url: requestURL request
                            , body: RequestBody request
                            , headers: headers
                            , method: fromMaybe Method.GET (Method.fromString (requestMethod request))
                            , contentLength: parseContentLength headers >>= fromString
                            }
                 , response: { writer: HttpResponse StatusLineOpen response }
                 , components: {}
                 }
      in catchException onRequestError (void (launchAff (middleware conn)))
