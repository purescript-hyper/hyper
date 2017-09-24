module Hyper.Node.FileServer (fileServer) where

import Prelude
import Node.Buffer as Buffer
import Node.Path as Path
import Control.IxMonad (ibind, (:>>=))
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class ResponseWritable, class Response, ResponseEnded, StatusLineOpen, end, headers, send, toResponse, writeStatus)
import Hyper.Status (statusOK)
import Node.Buffer (BUFFER, Buffer)
import Node.FS (FS)
import Node.FS.Aff (readFile, stat, exists)
import Node.FS.Stats (isDirectory, isFile)
import Node.Path (FilePath)
import Data.Array (null)

serveFile
  :: forall m e req res c b
  .  Monad m
  => MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => ResponseWritable b m Buffer
  => Response res m b
  => FilePath
  -> Array (Tuple String String)
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
serveFile path userProvidedHeaders = do
  buf <- lift' (liftAff (readFile path))
  contentLength <- liftEff (Buffer.size buf)
  let h = [ Tuple "Content-Type" "*/*; charset=utf-8"
          , Tuple "Content-Length" (show contentLength)
          ]
  _ <- writeStatus statusOK
  _ <- headers $ if null userProvidedHeaders then h else userProvidedHeaders
  response <- toResponse buf
  _ <- send response
  end
  where bind = ibind

-- | Extremly basic implementation of static file serving. Needs more love.
fileServer
  :: forall m e req res c b
  .  Monad m
  => MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => Request req m
  => ResponseWritable b m Buffer
  => Response res m b
  => FilePath
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
  -> Array (Tuple String String)
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
fileServer dir on404 headers = do
  conn ← getConn
  { url } <- getRequestData
  serve (Path.concat [dir, url])
  where
    serveStats absolutePath stats
      | isFile stats = serveFile absolutePath headers
      | isDirectory stats = serve (Path.concat [absolutePath, "index.html"])
      | otherwise = on404

    serve absolutePath = do
      fExists ← lift' (liftAff (exists absolutePath))
      if fExists
        then lift' (liftAff (stat absolutePath)) :>>= serveStats absolutePath
        else on404

    bind = ibind
