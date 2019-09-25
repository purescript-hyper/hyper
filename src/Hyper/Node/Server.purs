module Hyper.Node.Server
       ( HttpRequest
       , HttpResponse
       , NodeResponse
       , writeString
       , write
       , module Hyper.Node.Server.Options
       , runServer
       , runServer'
       ) where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (ipure, (:>>=))
import Effect.Aff (Aff, launchAff, launchAff_, makeAff, nonCanceler, runAff_)
import Effect.Aff.AVar (empty, new, put, take)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (catchException)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Node.Server.Options (Options)
import Hyper.Node.Server.Options
  ( Hostname(..)
  , Options
  , Port(..)
  , defaultOptions
  , defaultOptionsWithLogging
  ) as Hyper.Node.Server.Options
import Hyper.Request (class ReadableBody, class Request, class StreamableBody, RequestData, parseUrl, readBody)
import Hyper.Response (class ResponseWritable, class Response, ResponseEnded, StatusLineOpen)
import Hyper.Status (Status(..))
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (Stream, Writable)
import Node.Stream as Stream


data HttpRequest
  = HttpRequest HTTP.Request RequestData


instance requestHttpRequest :: Monad m => Request HttpRequest m where
  getRequestData = do
    getConn :>>=
      case _ of
        { request: HttpRequest _ d } -> ipure d


-- A limited version of Writable () e, with which you can only write, not end,
-- the Stream.
newtype NodeResponse m
  = NodeResponse (Writable () -> m Unit)

writeString :: forall m. MonadAff m => Encoding -> String -> NodeResponse m
writeString enc str = NodeResponse $ \w ->
  liftAff (makeAff (\k -> Stream.writeString w enc str (k (pure unit))
                          *> pure nonCanceler))

write :: forall m. MonadAff m => Buffer -> NodeResponse m
write buffer = NodeResponse $ \w ->
  liftAff (makeAff (\k -> Stream.write w buffer (k (pure unit))
                          *> pure nonCanceler))

instance stringNodeResponse :: MonadAff m => ResponseWritable (NodeResponse m) m String where
  toResponse = ipure <<< writeString UTF8

instance stringAndEncodingNodeResponse :: MonadAff m => ResponseWritable (NodeResponse m) m (Tuple String Encoding) where
  toResponse (Tuple body encoding) =
    ipure (writeString encoding body)

instance bufferNodeResponse :: MonadAff m
                                  => ResponseWritable (NodeResponse m) m Buffer where
  toResponse buf =
    ipure (write buf)

-- Helper function that reads a Stream into a Buffer, and throws error
-- in `Aff` when failed.
readBodyAsBuffer
  :: HttpRequest
  -> Aff Buffer
readBodyAsBuffer (HttpRequest request _) = do
  let stream = HTTP.requestAsStream request
  bodyResult <- empty
  chunks <- new []
  fillResult <- liftEffect $
    catchException (pure <<< Left) (Right <$> fillBody stream chunks bodyResult)
  -- Await the body, or an error.
  body <- take bodyResult
  -- Return the body, if neither `fillResult` nor `body` is a `Left`.
  either throwError pure (fillResult *> body)
  where
    fillBody stream chunks bodyResult = do
      -- Append all chunks to the body buffer.
      Stream.onData stream \chunk ->
        let modification = do
              v <- take chunks
              put (v <> [chunk]) chunks
        in void (launchAff modification)
      -- Complete with `Left` on error.
      Stream.onError stream $
        launchAff_ <<< flip put bodyResult <<< Left
      -- Complete with `Right` on successful "end" event.
      Stream.onEnd stream $ void $ launchAff $
        take chunks
        >>= concat'
        >>= (pure <<< Right)
        >>= flip put bodyResult
    concat' = liftEffect <<< Buffer.concat

instance readableBodyHttpRequestString :: (Monad m, MonadAff m)
                                       => ReadableBody HttpRequest m String where
  readBody =
    readBody :>>= (\(buffer :: Buffer) -> liftEffect $ Buffer.toString UTF8 buffer)

instance readableBodyHttpRequestBuffer :: (Monad m, MonadAff m)
                                       => ReadableBody HttpRequest m Buffer where
  readBody =
    _.request <$> getConn :>>=
    case _ of
      r -> liftAff (readBodyAsBuffer r)

instance streamableBodyHttpRequestReadable :: MonadAff m
                                           => StreamableBody
                                              HttpRequest
                                              m
                                              (Stream (read :: Stream.Read)) where
  streamBody =
    _.request <$> getConn :>>=
    case _ of
      HttpRequest request _ -> ipure (HTTP.requestAsStream request)

-- TODO: Make a newtype
data HttpResponse state = HttpResponse HTTP.Response

getWriter :: forall req res c m rw.
            Monad m =>
            Middleware
            m
            (Conn req { writer :: rw | res } c)
            (Conn req { writer :: rw | res } c)
            rw
getWriter = _.response.writer <$> getConn

setStatus :: forall req res c m.
            MonadEffect m
          => Status
          -> HTTP.Response
          -> Middleware m (Conn req res c) (Conn req res c) Unit
setStatus (Status { code, reasonPhrase }) r = liftEffect do
  HTTP.setStatusCode r code
  HTTP.setStatusMessage r reasonPhrase

writeHeader' :: forall req res c m.
               MonadEffect m
             => (Tuple String String)
             -> HTTP.Response
             -> Middleware m (Conn req res c) (Conn req res c) Unit
writeHeader' (Tuple name value) r =
  liftEffect $ HTTP.setHeader r name value

writeResponse :: forall req res c m.
                MonadAff m
             => HTTP.Response
             -> NodeResponse m
             -> Middleware m (Conn req res c) (Conn req res c) Unit
writeResponse r (NodeResponse f) =
  lift' (f (HTTP.responseAsStream r))

endResponse :: forall req res c m.
              MonadEffect m
            => HTTP.Response
            -> Middleware m (Conn req res c) (Conn req res c) Unit
endResponse r =
  liftEffect (Stream.end (HTTP.responseAsStream r) (pure unit))

instance responseWriterHttpResponse :: MonadAff m
                                    => Response HttpResponse m (NodeResponse m) where
  writeStatus status = Ix.do
    { response: HttpResponse r } <- getConn
    setStatus status r
    modifyConn (_ { response = HttpResponse r })

  writeHeader header = Ix.do
    { response: HttpResponse r } <- getConn
    writeHeader' header r
    modifyConn (_ { response = HttpResponse r })

  closeHeaders = Ix.do
    { response: HttpResponse r } <- getConn
    modifyConn (_ { response = HttpResponse r })

  send f = Ix.do
    { response: HttpResponse r } <- getConn
    writeResponse r f
    modifyConn (_ { response = HttpResponse r })

  end = Ix.do
    { response: HttpResponse r } <- getConn
    endResponse r
    modifyConn (_ { response = HttpResponse r })


mkHttpRequest :: HTTP.Request -> HttpRequest
mkHttpRequest request =
  HttpRequest request requestData
  where
    headers = HTTP.requestHeaders request
    requestData =
      { url: HTTP.requestURL request
      , parsedUrl: defer \_ -> parseUrl (HTTP.requestURL request)
      , headers: headers
      , method: Method.fromString (HTTP.requestMethod request)
      , contentLength: Object.lookup "content-length" headers
                      >>= Int.fromString
      }


runServer'
  :: forall m c c'
   . Functor m
  => Options
  -> c
  -> (forall a. m a -> Aff a)
  -> Middleware
     m
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Effect Unit
runServer' options components runM middleware = do
  server <- HTTP.createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: unwrap options.hostname
                      , backlog: Nothing
                      }
  HTTP.listen server listenOptions (options.onListening options.hostname options.port)
  where
    onRequest :: HTTP.Request -> HTTP.Response -> Effect Unit
    onRequest request response =
      let conn = { request: mkHttpRequest request
                 , response: HttpResponse response
                 , components: components
                 }
          callback =
            case _ of
              Left err -> options.onRequestError err
              Right _ -> pure unit
      in conn
         # evalMiddleware middleware
         # runM
         # runAff_ callback

runServer
  :: forall c c'.
     Options
  -> c
  -> Middleware
     Aff
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  -> Effect Unit
runServer options components middleware =
  runServer' options components identity middleware
