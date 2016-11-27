module Hyper.Router ( Path
                    , pathToHtml
                    , pathFromString
                    , Supported
                    , Unsupported
                    , ResourceMethod
                    , handler
                    , notSupported
                    , resource
                    , fallbackTo
                    ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(MaybeT))
import Data.Array (filter)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), split, joinWith)
import Hyper.Conn (Conn)
import Hyper.Method (Method(POST, GET))
import Hyper.Middleware (Middleware, runMiddlewareT, MiddlewareT(MiddlewareT))

type Path = Array String

pathToHtml :: Path -> String
pathToHtml = (<>) "/" <<< joinWith "/"

pathFromString :: String -> Path
pathFromString = filter ((/=) "") <<< split (Pattern "/")

data Supported = Supported
data Unsupported = Unsupported

data ResourceMethod r e x y
  = Routed (Middleware e x y) r (r ~ Supported)
  | NotRouted r (r ~ Unsupported)

handler :: forall e req req' res res' c c'.
           Middleware e (Conn req res c) (Conn req' res' c')
           -> ResourceMethod Supported e (Conn req res c) (Conn req' res' c')
handler mw = Routed mw Supported id

notSupported :: forall e req req' res res' c c'.
                ResourceMethod Unsupported e (Conn req res c) (Conn req' res' c')
notSupported = NotRouted Unsupported id

foreign import _router :: forall ms e req req' res res' c c'.
                          { path :: Path | ms }
                       -> String
                       -> Middleware e (Conn req res c) (Conn req' res' c')

methodHandler :: forall m e x y.
                 ResourceMethod m e x y
                 -> Maybe (Middleware e x y)
methodHandler (Routed mw _ _) = Just mw
methodHandler (NotRouted _ _) = Nothing

resource :: forall gr pr e req req' res res' c c'.
            { path :: Path
            , "GET" :: ResourceMethod
                       gr
                       e
                       (Conn { path :: Path, method :: Method | req } res c)
                       (Conn { path :: Path, method :: Method | req' } res' c')
            , "POST" :: ResourceMethod
                        pr
                        e
                        (Conn { path :: Path, method :: Method | req } res c)
                        (Conn { path :: Path, method :: Method | req' } res' c')
            }
         -> MiddlewareT
            (MaybeT (Aff e))
            (Conn { path :: Path, method :: Method | req } res c)
            (Conn { path :: Path, method :: Method | req' } res' c')
resource r =
  MiddlewareT (MaybeT <$> result)
  where
    handler' conn =
      case conn.request.method of
        GET -> methodHandler r."GET"
        POST -> methodHandler r."POST"
    result conn =
      if r.path == conn.request.path
      then case handler' conn of
        Just mw -> Just <$> runMiddlewareT mw conn
        Nothing -> pure Nothing
      else pure Nothing

fallbackTo :: forall e req req' res res' c c'.
              Middleware e (Conn req res c) (Conn req' res' c')
              -> MiddlewareT (MaybeT (Aff e)) (Conn req res c) (Conn req' res' c')
              -> Middleware e (Conn req res c) (Conn req' res' c')
fallbackTo fallback mw = MiddlewareT $ \conn -> do
  result <- runMaybeT $ runMiddlewareT mw conn
  case result of
    Just conn' -> pure conn'
    Nothing -> runMiddlewareT fallback conn
  
