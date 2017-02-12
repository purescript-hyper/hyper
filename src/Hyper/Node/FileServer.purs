module Hyper.Node.FileServer (fileServer) where

import Prelude
import Node.Buffer as Buffer
import Node.Path as Path
import Control.IxMonad (ibind, (:>>=))
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn)
import Hyper.Core (class ResponseWriter, writeStatus, ResponseEnded, StatusLineOpen)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Response (class Response, respond, headers)
import Hyper.Status (statusOK)
import Node.Buffer (Buffer, BUFFER)
import Node.FS (FS)
import Node.FS.Aff (readFile, stat, exists)
import Node.FS.Stats (isDirectory, isFile)
import Node.Path (FilePath)

serveFile
  :: forall m e rw b req res c.
     ( Monad m
     , MonadAff (fs :: FS, buffer :: BUFFER | e) m
     , Response b m Buffer
     , ResponseWriter rw m b
     ) =>
     FilePath
  -> Middleware
     m
     (Conn { url :: String | req } { writer :: rw StatusLineOpen |  res } c)
     (Conn { url :: String | req } { writer :: rw ResponseEnded | res } c)
     Unit
serveFile path = do
  buf <- lift' (liftAff (readFile path))
  contentLength <- liftEff (Buffer.size buf)
  writeStatus statusOK
  headers [ Tuple "Content-Type" "*/*; charset=utf-8"
          , Tuple "Content-Length" (show contentLength)
          ]
  respond buf
  where bind = ibind

-- | Extremly basic implementation of static file serving. Needs more love.
fileServer
  :: forall m e rw b req res c.
     ( Monad m
     , MonadAff (fs :: FS, buffer :: BUFFER | e) m
     , Response b m Buffer
     , ResponseWriter rw m b
     ) =>
     FilePath
  -> Middleware
     m
     (Conn { url :: String | req } { writer :: rw StatusLineOpen |  res } c)
     (Conn { url :: String | req } { writer :: rw ResponseEnded | res } c)
     Unit
  -> Middleware
     m
     (Conn { url :: String | req } { writer :: rw StatusLineOpen |  res } c)
     (Conn { url :: String | req } { writer :: rw ResponseEnded | res } c)
     Unit
fileServer dir on404 = do
  conn ← getConn
  serve (Path.concat [dir, conn.request.url])
  where
    serveStats absolutePath stats
      | isFile stats = serveFile absolutePath
      | isDirectory stats = serve (Path.concat [absolutePath, "index.html"])
      | otherwise = on404

    serve absolutePath = do
      fExists ← lift' (liftAff (exists absolutePath))
      if fExists
        then lift' (liftAff (stat absolutePath)) :>>= serveStats absolutePath
        else on404

    bind = ibind
