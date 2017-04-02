module Hyper.Node.Server
       ( HttpRequest
       , HttpResponse
       , NodeResponseWriter
       , writeString
       , write
       , defaultOptions
       , defaultOptionsWithLogging
       , runServer
       , runServer'
       ) where

import Prelude
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.StrMap as StrMap
import Node.HTTP as HTTP
import Node.Stream as Stream
import Control.IxMonad (ipure, (:*>), (:>>=))
import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Aff.AVar (putVar, takeVar, modifyVar, makeVar', AVAR, makeVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, catchException, error)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Port (Port(..))
import Hyper.Request (class ReadableBody, class Request, RequestData)
import Hyper.Response (class ResponseWritable, class ResponseWriter, ResponseEnded, StatusLineOpen)
import Hyper.Status (Status(..))
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP)
import Node.Stream (Writable)


data HttpRequest
  = HttpRequest HTTP.Request RequestData


instance requestHttpRequest :: Monad m => Request HttpRequest m where
  getRequestData = do
    getConn :>>=
    case _ of
      { request: HttpRequest _ d } -> ipure d


-- A limited version of Writable () e, with which you can only write, not end,
-- the Stream.
newtype NodeResponseWriter m e
  = NodeResponseWriter (Writable () e -> m Unit)

writeString :: forall m e. MonadAff e m => Encoding -> String -> NodeResponseWriter m e
writeString enc str = NodeResponseWriter $ \w -> liftAff (makeAff (writeAsAff w))
  where
    writeAsAff w fail succeed =
      Stream.writeString w enc str (succeed unit) >>=
      if _
        then succeed unit
        else fail (error "Failed to write string to response")

write :: forall m e. MonadAff e m => Buffer -> NodeResponseWriter m e
write buffer = NodeResponseWriter $ \w ->
  liftAff (makeAff (\fail succeed -> void $ Stream.write w buffer (succeed unit)))

instance stringNodeResponseWriter :: (MonadAff e m) => ResponseWritable (NodeResponseWriter m e) m String where
  toResponse = ipure <<< writeString UTF8

instance stringAndEncodingNodeResponseWriter :: (MonadAff e m) => ResponseWritable (NodeResponseWriter m e) m (Tuple String Encoding) where
  toResponse (Tuple body encoding) =
    ipure (writeString encoding body)

instance bufferNodeResponseWriter :: (MonadAff e m)
                                  => ResponseWritable (NodeResponseWriter m e) m Buffer where
  toResponse buf =
    ipure (write buf)

readBody
  :: forall e.
     HttpRequest
  -> Aff (http :: HTTP, avar :: AVAR | e) String
readBody (HttpRequest request _) = do
  let stream = HTTP.requestAsStream request
  completeBody <- makeVar
  chunks <- makeVar' ""
  e <- liftEff (catchException (pure <<< Just) (fillBody stream chunks completeBody *> pure Nothing))
  case e of
    Just err -> throwError err
    Nothing -> takeVar completeBody
  where
    fillBody stream chunks completeBody = do
      Stream.onDataString stream UTF8 \chunk -> void do
        launchAff (modifyVar (_ <> chunk) chunks)
      Stream.onEnd stream $ void (launchAff (takeVar chunks >>= putVar completeBody))

instance requestBodyReaderReqestBody :: (Monad m, MonadAff (http :: HTTP, avar :: AVAR | e) m)
                                     => ReadableBody HttpRequest m String where
  readBody =
    _.request <$> getConn :>>=
    case _ of
      r -> lift' (liftAff (readBody r))

-- TODO: Make a newtype
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
    getConn :>>= \{ response: HttpResponse r } ->
      setStatus status r
      :*> modifyConn (_ { response = HttpResponse r })

  writeHeader header =
    getConn :>>= \{ response: HttpResponse r } ->
      writeHeader' header r
      :*> modifyConn (_ { response = HttpResponse r })

  closeHeaders =
    getConn :>>= \{ response: HttpResponse r } ->
      modifyConn (_ { response = HttpResponse r })

  send f =
    getConn :>>= \{ response: HttpResponse r } ->
      writeResponse r f
      :*> modifyConn (_ { response = HttpResponse r })

  end =
    getConn :>>= \{ response: HttpResponse r } ->
      endResponse r
      :*> modifyConn (_ { response = HttpResponse r })


type ServerOptions e =
  { hostname ∷ String
  , port ∷ Port
  , onListening ∷ Port → Eff (http ∷ HTTP | e) Unit
  , onRequestError ∷ Error → Eff (http ∷ HTTP | e) Unit
  }


defaultOptions ∷ ∀ e. ServerOptions e
defaultOptions =
  { hostname: "0.0.0.0"
  , port: Port 3000
  , onListening: const (pure unit)
  , onRequestError: const (pure unit)
  }


defaultOptionsWithLogging ∷ ∀ e. ServerOptions (console ∷ CONSOLE | e)
defaultOptionsWithLogging =
  defaultOptions { onListening = onListening
                 , onRequestError = onRequestError
                 }
  where
    onListening (Port port) =
      log ("Listening on http://localhost:" <> show port)
    onRequestError err =
      log ("Request failed: " <> show err)


mkHttpRequest :: HTTP.Request -> HttpRequest
mkHttpRequest request =
  HttpRequest request requestData
  where
    headers = HTTP.requestHeaders request
    requestData =
      { url: HTTP.requestURL request
      , headers: headers
      , method: Method.fromString (HTTP.requestMethod request)
      , contentLength: StrMap.lookup "content-length" headers
                      >>= Int.fromString
      }


runServer'
  :: forall m e c c'
   . Functor m
  => ServerOptions e
  -> c
  -> (forall a. m a -> Aff (http :: HTTP | e) a)
  -> Middleware
     m
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Eff (http :: HTTP | e) Unit
runServer' options components runM middleware = do
  server <- HTTP.createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: "0.0.0.0"
                      , backlog: Nothing
                      }
  HTTP.listen server listenOptions (options.onListening options.port)
  where
    onRequest ∷ HTTP.Request → HTTP.Response → Eff (http :: HTTP | e) Unit
    onRequest request response =
      let conn = { request: mkHttpRequest request
                 , response: HttpResponse response
                 , components: components
                 }
      in catchException options.onRequestError (void (launchAff (runM (evalMiddleware middleware conn))))


runServer
  :: forall e c c'.
     ServerOptions e
  -> c
  -> Middleware
     (Aff (http :: HTTP | e))
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Eff (http :: HTTP | e) Unit
runServer options components middleware =
  runServer' options components id middleware
