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

import Prelude
import Node.HTTP (HTTP)
import Node.HTTP as HTTP
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.StrMap as StrMap
import Node.Stream as Stream
import Control.IxMonad (ibind, ipure, (:*>), (:>>=))
import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Aff.AVar (putVar, takeVar, modifyVar, makeVar', AVAR, makeVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, Error)
import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn, putConn)
import Hyper.Port (Port(..))
import Hyper.Response (class Response, class ResponseWriter, ResponseEnded, StatusLineOpen)
import Hyper.Status (Status(..))
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.Stream (Writable)


newtype RequestBody = RequestBody HTTP.Request

derive instance newtypeRequestBody :: Newtype RequestBody _


-- A limited version of Writable () e, with which you can only write, not end,
-- the Stream.
newtype NodeResponseWriter m e
  = NodeResponseWriter (Writable () e -> m Unit)

writeString :: forall m e. MonadAff e m => Encoding -> String -> NodeResponseWriter m e
writeString enc str = NodeResponseWriter $ \w ->
  liftAff (makeAff (\fail succeed -> void $ Stream.writeString w enc str (succeed unit)))

write :: forall m e. MonadAff e m => Buffer -> NodeResponseWriter m e
write buffer = NodeResponseWriter $ \w ->
  liftAff (makeAff (\fail succeed -> void $ Stream.write w buffer (succeed unit)))

instance stringNodeResponseWriter :: (MonadAff e m) => Response (NodeResponseWriter m e) m String where
  toResponse = ipure <<< writeString UTF8

instance stringAndEncodingNodeResponseWriter :: (MonadAff e m) => Response (NodeResponseWriter m e) m (Tuple String Encoding) where
  toResponse (Tuple body encoding) =
    ipure (writeString encoding body)

instance bufferNodeResponseWriter :: (MonadAff e m)
                                  => Response (NodeResponseWriter m e) m Buffer where
  toResponse buf =
    ipure (write buf)

readBody :: forall e. RequestBody -> Aff (http :: HTTP, err :: EXCEPTION, avar :: AVAR | e) String
readBody body = do
  let stream = HTTP.requestAsStream (unwrap body)
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

data HttpResponse state = HttpResponse HTTP.Response

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
          → HTTP.Response
          → Middleware m (Conn req res c) (Conn req res c) Unit
setStatus (Status { code, reasonPhrase }) r = liftEff do
  HTTP.setStatusCode r code
  HTTP.setStatusMessage r reasonPhrase

writeHeader' ∷ ∀ req res c m e.
               MonadEff (http ∷ HTTP | e) m
             ⇒ (Tuple String String)
             → HTTP.Response
             → Middleware m (Conn req res c) (Conn req res c) Unit
writeHeader' (Tuple name value) r =
  liftEff $ HTTP.setHeader r name value

writeResponse ∷ ∀ req res c m e.
                MonadAff (http ∷ HTTP | e) m
             ⇒ HTTP.Response
             → NodeResponseWriter m (http :: HTTP | e)
             → Middleware m (Conn req res c) (Conn req res c) Unit
writeResponse r (NodeResponseWriter f) =
  lift' (f (HTTP.responseAsStream r))

endResponse ∷ ∀ req res c m e.
              MonadEff (http ∷ HTTP | e) m
            ⇒ HTTP.Response
            → Middleware m (Conn req res c) (Conn req res c) Unit
endResponse r =
  liftEff (Stream.end (HTTP.responseAsStream r) (pure unit))

instance responseWriterHttpResponse :: MonadAff (http ∷ HTTP | e) m
                                    => ResponseWriter HttpResponse m (NodeResponseWriter m (http :: HTTP | e)) where
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

  send f =
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

type ServerOptions m e =
  { hostname ∷ String
  , port ∷ Port
  , onListening ∷ Port → Eff (http ∷ HTTP | e) Unit
  , onRequestError ∷ Error → Eff (http ∷ HTTP | e) Unit
  , runM ∷ ∀ a. m a → Aff (http ∷ HTTP | e) a
  }


defaultOptions ∷ ∀ e. ServerOptions (Aff (http ∷ HTTP | e)) e
defaultOptions = { hostname: "0.0.0.0"
                 , port: Port 3000
                 , onListening: const (pure unit)
                 , onRequestError: const (pure unit)
                 , runM: id
                 }


runServer
  :: forall m e req res c c'.
     Functor m ⇒
     ServerOptions m e
  -> c
  -> Middleware
     m
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
runServer options components middleware = do
  server <- HTTP.createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: "0.0.0.0"
                      , backlog: Nothing
                      }
  HTTP.listen server listenOptions (options.onListening options.port)
  where
    parseContentLength headers = StrMap.lookup "content-length" headers
    onRequest ∷ HTTP.Request → HTTP.Response → Eff (http :: HTTP | e) Unit
    onRequest request response =
      let headers = HTTP.requestHeaders request
          conn = { request: { url: HTTP.requestURL request
                            , body: RequestBody request
                            , headers: headers
                            , method: Method.fromString (HTTP.requestMethod request)
                            , contentLength: parseContentLength headers >>= Int.fromString
                            }
                 , response: { writer: HttpResponse response }
                 , components: components
                 }
      in catchException options.onRequestError (void (launchAff (options.runM (evalMiddleware middleware conn))))
