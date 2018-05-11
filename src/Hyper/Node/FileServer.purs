module Hyper.Node.FileServer (fileServer) where

import Prelude

import Control.IxMonad (ibind, (:>>=))
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (last)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (maybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class ResponseWritable, class Response, ResponseEnded, StatusLineOpen, end, headers, send, toResponse, writeStatus)
import Hyper.Status (statusOK)
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.FS (FS)
import Node.FS.Aff (readFile, stat, exists)
import Node.FS.Stats (isDirectory, isFile)
import Node.Path (FilePath)
import Node.Path as Path

htaccess :: Map String String
htaccess = fromFoldable $
  [ Tuple "aab" "application/x-authorware-bin"
  , Tuple "aam" "application/x-authorware-map"
  , Tuple "aas" "application/x-authorware-seg"
  , Tuple "asf" "video/x-ms-asf"
  , Tuple "asp" "text/html"
  , Tuple "asx" "video/x-ms-asf"
  , Tuple "class" "application/x-java-applet"
  , Tuple "css" "text/css"
  , Tuple "dcr" "application/x-director"
  , Tuple "dir" "application/x-director"
  , Tuple "dmg" "application/octet-stream"
  , Tuple "dxr" "application/x-director"
  , Tuple "fgd" "application/x-director"
  , Tuple "fh" "image/x-freehand"
  , Tuple "fh4" "image/x-freehand"
  , Tuple "fh5" "image/x-freehand"
  , Tuple "fh7" "image/x-freehand"
  , Tuple "fhc" "image/x-freehand"
  , Tuple "gtar" "application/x-gtar"
  , Tuple "gz" "application/x-gzip"
  , Tuple "ico" "image/vnd.microsoft.icon"
  , Tuple "m3u" "audio/x-mpegurl"
  , Tuple "mov" "video/quicktime"
  , Tuple "pdf" "application/pdf"
  , Tuple "qt" "video/quicktime"
  , Tuple "ra" "audio/vnd.rn-realaudio"
  , Tuple "ram" "audio/vnd.rn-realaudio"
  , Tuple "rar" "application/x-rar-compressed"
  , Tuple "rm" "application/vnd.rn-realmedia"
  , Tuple "rpm" "audio/x-pn-realaudio-plugin"
  , Tuple "rv" "video/vnd.rn-realvideo"
  , Tuple "shtml" "text/html"
  , Tuple "svg" "image/svg+xml"
  , Tuple "svgz" "image/svg+xml"
  , Tuple "swf" "application/x-shockwave-flash"
  , Tuple "torrent" "application/x-bittorrent"
  , Tuple "wav" "audio/x-wav"
  , Tuple "wax" "audio/x-ms-wax"
  , Tuple "wm" "video/x-ms-wm"
  , Tuple "wma" "audio/x-ms-wma"
  , Tuple "wmd" "application/x-ms-wmd"
  , Tuple "wmv" "video/x-ms-wmv"
  , Tuple "wmx" "video/x-ms-wmx"
  , Tuple "wmz" "application/x-ms-wmz"
  , Tuple "wvx" "video/x-ms-wvx"
  , Tuple "xbm" "image/x-xbitmap"
  , Tuple "xhtml" "application/xhtml+xml"
  , Tuple "xml" "text/xml"
  , Tuple "zip" "application/zip"
  ]

serveFile
  :: forall m e req res c b
  .  Monad m
  => MonadAff (fs :: FS, buffer :: BUFFER | e) m
  => ResponseWritable b m Buffer
  => Response res m b
  => FilePath
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
serveFile path = do
  let
    ext = last $ split (Pattern ".") path
    contentType = maybe "*/*" id (ext >>= flip lookup htaccess)
  buf <- lift' (liftAff (readFile path))
  contentLength <- liftEff (Buffer.size buf)
  _ <- writeStatus statusOK
  _ <- headers [ Tuple "Content-Type" (contentType <> "; charset=utf-8")
          , Tuple "Content-Length" (show contentLength)
          ]
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
  -> Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
fileServer dir on404 = do
  conn ← getConn
  { url } <- getRequestData
  serve (Path.concat [dir, url])
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
