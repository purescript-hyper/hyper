module Hyper.Node.FileServer where

import Node.Buffer as Buffer
import Node.Path as Path
import Control.Applicative (pure)
import Control.Bind (ifM, (>>=), bind)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Boolean (otherwise)
import Data.Function ((#))
import Data.Functor (map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show (show)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (class ResponseWriter, statusOK, writeStatus, ResponseEnded, StatusLineOpen, Conn, Middleware)
import Hyper.Response (respond, headers)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (stat, readTextFile, exists)
import Node.FS.Stats (isDirectory, isFile)
import Node.Path (FilePath)

-- | Extremly basic implementation of static file serving. Needs more love.
fileServer
  :: forall m e rw req res c.
     (MonadAff (fs :: FS, buffer :: BUFFER | e) m, ResponseWriter rw m) =>
     FilePath
  -> Middleware
     m
     (Conn { url :: String | req } { writer :: rw StatusLineOpen |  res } c)
     (Maybe (Conn { url :: String | req } { writer :: rw ResponseEnded | res } c))
fileServer dir conn@{ request: { url: url } } = do
  serve (Path.concat [dir, url])
  where
    serveFile path = do
      contents <- liftAff (readTextFile UTF8 path)
      buf <- liftAff (liftEff (Buffer.fromString contents UTF8))
      contentLength <- liftAff (liftEff (Buffer.size buf))
      writeStatus statusOK conn
        >>= headers [ Tuple "Content-Type" "*/*; charset=utf-8"
                    , Tuple "Content-Length" (show contentLength)
                    ]
        >>= respond contents
        # map Just

    serveStats absolutePath stats
      | isFile stats = serveFile absolutePath
      | isDirectory stats = serve (Path.concat [absolutePath, "index.html"])
      | otherwise = pure Nothing

    serve absolutePath =
      ifM (liftAff (exists absolutePath))
      (liftAff (stat absolutePath) >>= serveStats absolutePath)
      (pure Nothing)
