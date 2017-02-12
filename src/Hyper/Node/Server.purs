module Hyper.Node.Server
       ( RequestBody
       , HttpResponse
       , readBodyAsString
       , defaultOptions
       , runServer
       )where

import Node.HTTP
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.StrMap as StrMap
import Node.Buffer as Buffer
import Node.Stream as Stream
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.IxMonad (ibind, (:>>=))
import Control.Monad (class Monad, void, (>>=))
import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Aff.AVar (putVar, takeVar, modifyVar, makeVar', AVAR, makeVar)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, Error)
import Data.Either (Either)
import Data.Function (($), (<<<))
import Data.Functor (map, (<$>))
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup ((<>))
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Hyper.Conn (Conn)
import Hyper.Core (class ResponseWriter, Port(..), ResponseEnded, StatusLineOpen)
import Hyper.Middleware (Middleware, evalMiddleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn, putConn)
import Hyper.Response (class Response)
import Hyper.Status (Status(..))
import Node.Buffer (BUFFER, Buffer)
import Node.Encoding (Encoding(..))


newtype RequestBody = RequestBody Request

derive instance newtypeRequestBody :: Newtype RequestBody _


newtype ResponseBody = ResponseBody Buffer

derive instance newtypeResponseBody :: Newtype ResponseBody _

instance stringResponseBody :: (MonadAff (buffer :: BUFFER | e) m) => Response ResponseBody m String where
  toResponse body =
    liftEff (map ResponseBody (Buffer.fromString body UTF8))

instance stringAndEncodingResponseBody :: (MonadAff (buffer :: BUFFER | e) m) => Response ResponseBody m (Tuple String Encoding) where
  toResponse (Tuple body encoding) =
    liftEff (map ResponseBody (Buffer.fromString body encoding))

instance bufferResponseBody :: Monad m => Response ResponseBody m Buffer where
  toResponse = pure <<< ResponseBody

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
                   Unit
readBodyAsString = do
  conn ← getConn
  s <- lift' (readBody conn.request.body)
  putConn (conn { request { body = s } })
  where bind = ibind

data HttpResponse state = HttpResponse Response

getWriter ∷ ∀ req res c m rw.
            Monad m ⇒
            Middleware
            m
            (Conn req { writer ∷ rw | res } c)
            (Conn req { writer ∷ rw | res } c)
            rw
getWriter = _.response.writer <$> getConn

setStatus ∷ ∀ req res c m e.
            MonadEff (http ∷ HTTP | e) m
          ⇒ Status
          → Response
          → Middleware m (Conn req res c) (Conn req res c) Unit
setStatus (Status { code, reasonPhrase }) r = liftEff do
  setStatusCode r code
  setStatusMessage r reasonPhrase

writeHeader' ∷ ∀ req res c m e.
               MonadEff (http ∷ HTTP | e) m
             ⇒ (Tuple String String)
             → Response
             → Middleware m (Conn req res c) (Conn req res c) Unit
writeHeader' (Tuple name value) r =
  liftEff $ setHeader r name value

writeResponse ∷ ∀ req res c m e.
                MonadEff (http ∷ HTTP | e) m
             ⇒ Response
             → Buffer
             → Middleware m (Conn req res c) (Conn req res c) Unit
writeResponse r b =
  void (liftEff (Stream.write (responseAsStream r) b (pure unit)))

endResponse ∷ ∀ req res c m e.
              MonadEff (http ∷ HTTP | e) m
            ⇒ Response
            → Middleware m (Conn req res c) (Conn req res c) Unit
endResponse r =
  liftEff (Stream.end (responseAsStream r) (pure unit))

instance responseWriterHttpResponse :: MonadEff (http ∷ HTTP | e) m => ResponseWriter HttpResponse m ResponseBody where
  writeStatus status = do
    getWriter :>>=
    case _ of
      HttpResponse r → do
        setStatus status r
        modifyConn (_ { response { writer = HttpResponse r }})
    where bind = ibind

  writeHeader header = do
    getWriter :>>=
    case _ of
      HttpResponse r → do
        writeHeader' header r
        modifyConn (_ { response { writer = HttpResponse r }})
    where bind = ibind

  closeHeaders =
    getWriter :>>=
    case _ of
      HttpResponse r →
        modifyConn (_ { response { writer = HttpResponse r }})
    where bind = ibind

  send (ResponseBody b) =
    getWriter :>>=
    case _ of
      HttpResponse r → do
        writeResponse r b
        modifyConn (_ { response { writer = HttpResponse r }})
    where bind = ibind

  end =
    getWriter :>>=
    case _ of
      HttpResponse r → do
        endResponse r
        modifyConn (_ { response { writer = HttpResponse r }})
    where bind = ibind

type ServerOptions = { hostname ∷ String
                     , port ∷ Port
                     }


defaultOptions ∷ ServerOptions
defaultOptions = { hostname: "0.0.0.0"
                 , port: Port 3000
                 }


runServer :: forall e req res c c'.
             ServerOptions
             -> (Port -> Eff (http ∷ HTTP | e) Unit)
             -> (Error -> Eff (http ∷ HTTP | e) Unit)
             -> c
             -> Middleware
                (Aff (http :: HTTP | e))
                (Conn { url :: String
                      , body :: RequestBody
                      , contentLength :: Maybe Int
                      , headers :: StrMap String
                      , method :: Either Method CustomMethod
                      }
                      { writer :: HttpResponse StatusLineOpen }
                      c)
                (Conn req { writer :: HttpResponse ResponseEnded | res } c')
                Unit
             -> Eff (http :: HTTP | e) Unit
runServer options onListening onRequestError components middleware = do
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
                            , method: Method.fromString (requestMethod request)
                            , contentLength: parseContentLength headers >>= Int.fromString
                            }
                 , response: { writer: HttpResponse response }
                 , components: components
                 }
      in catchException onRequestError (void (launchAff (evalMiddleware middleware conn)))
