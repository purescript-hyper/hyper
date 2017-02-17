module Hyper.Node.Server
       ( RequestBody
       , HttpResponse
       , NodeResponseWriter
       , writeString
       , write
       , readBodyAsString
       , defaultOptions
       , runServer
       )where

import Node.HTTP
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.StrMap as StrMap
import Node.Stream as Stream
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.IxMonad (ibind, ipure, (:*>), (:>>=))
import Control.Monad (class Monad, void, (>>=))
import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Aff.AVar (putVar, takeVar, modifyVar, makeVar', AVAR, makeVar)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, Error)
import Data.Either (Either)
import Data.Function (($), (<<<))
import Data.Functor ((<$>))
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup ((<>))
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn, putConn)
import Hyper.Port (Port(..))
import Hyper.Response (class Response, class ResponseWriter, ResponseEnded, StatusLineOpen)
import Hyper.Status (Status(..))
import Node.Buffer (BUFFER, Buffer)
import Node.Encoding (Encoding(..))
import Node.Stream (Writable)


newtype RequestBody = RequestBody Request

derive instance newtypeRequestBody :: Newtype RequestBody _


-- A limited version of Writable () e, with which you can only write, not end,
-- the Stream.
data NodeResponseWriter e
  = NodeResponseWriter (Writable () e -> Eff e Unit)

writeString :: forall e. Encoding -> String -> NodeResponseWriter e
writeString enc str = NodeResponseWriter (\w -> void (Stream.writeString w enc str (pure unit)))

write :: forall e. Buffer -> NodeResponseWriter (buffer :: BUFFER | e)
write buffer = NodeResponseWriter (\w -> void (Stream.write w buffer (pure unit)))

instance stringNodeResponseWriter :: (MonadAff e m) => Response (NodeResponseWriter e) m String where
  toResponse = ipure <<< writeString UTF8

instance stringAndEncodingNodeResponseWriter :: (MonadAff e m) => Response (NodeResponseWriter e) m (Tuple String Encoding) where
  toResponse (Tuple body encoding) =
    ipure (writeString encoding body)

instance bufferNodeResponseWriter :: Monad m => Response (NodeResponseWriter e) m Buffer where
  toResponse buf =
    ipure (NodeResponseWriter (\stream -> void (Stream.write stream buf (pure unit))))

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
             → (Writable () (http :: HTTP | e) -> Eff (http :: HTTP | e) Unit)
             → Middleware m (Conn req res c) (Conn req res c) Unit
writeResponse r f =
  void (liftEff (f (responseAsStream r)))

endResponse ∷ ∀ req res c m e.
              MonadEff (http ∷ HTTP | e) m
            ⇒ Response
            → Middleware m (Conn req res c) (Conn req res c) Unit
endResponse r =
  liftEff (Stream.end (responseAsStream r) (pure unit))

instance responseWriterHttpResponse :: MonadAff (http ∷ HTTP | e) m
                                    => ResponseWriter HttpResponse m (NodeResponseWriter (http :: HTTP | e)) where
  writeStatus status =
    getWriter :>>=
    case _ of
      HttpResponse r → do
        setStatus status r
        :*> modifyConn (_ { response { writer = HttpResponse r }})

  writeHeader header =
    getWriter :>>=
    case _ of
      HttpResponse r →
        writeHeader' header r
        :*> modifyConn (_ { response { writer = HttpResponse r }})

  closeHeaders =
    getWriter :>>=
    case _ of
      HttpResponse r →
        modifyConn (_ { response { writer = HttpResponse r }})

  send (NodeResponseWriter f) =
    getWriter :>>=
    case _ of
      HttpResponse r → do
        writeResponse r f
        :*> modifyConn (_ { response { writer = HttpResponse r }})

  end =
    getWriter :>>=
    case _ of
      HttpResponse r → do
        endResponse r
        :*> modifyConn (_ { response { writer = HttpResponse r }})

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
