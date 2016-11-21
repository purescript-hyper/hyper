module Hyper.Router ( Path
                    , pathToHtml
                    , pathFromString
                    , Supported
                    , Unsupported
                    , ResourceMethod
                    , handler
                    , notSupported
                    , resource
                    ) where

import Prelude
import Data.Array (filter)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), split, joinWith)
import Hyper.Conn (Middleware, PartialMiddleware)
import Hyper.Method (Method(POST, GET))

type Path = Array String

pathToHtml :: Path -> String
pathToHtml = (<>) "/" <<< joinWith "/"

pathFromString :: String -> Path
pathFromString = filter ((/=) "") <<< split (Pattern "/")

data Supported = Supported
data Unsupported = Unsupported

data ResourceMethod r e req req' res res' c c'
  = Routed (Middleware e req req' res res' c c') r (r ~ Supported)
  | NotRouted r (r ~ Unsupported)

handler :: forall e req req' res res' c c'.
           Middleware e req req' res res' c c'
           -> ResourceMethod Supported e req req' res res' c c'
handler mw = Routed mw Supported id

notSupported :: forall e req req' res res' c c'.
                ResourceMethod Unsupported e req req' res res' c c'
notSupported = NotRouted Unsupported id

foreign import _router :: forall ms e req req' res res' c c'.
                          { path :: Path | ms }
                       -> String
                       -> Middleware e req req' res res' c c'

methodHandler :: forall m e req req' res res' c c'.
                 ResourceMethod m e req req' res res' c c'
                 -> Maybe (Middleware e req req' res res' c c')
methodHandler (Routed mw _ _) = Just mw
methodHandler (NotRouted _ _) = Nothing

resource :: forall gr pr e req req' res res' c c'.
            { path :: Path
            , "GET" :: ResourceMethod
                       gr
                       e
                       { path :: Path, method :: Method | req }
                       req'
                       res
                       res'
                       c
                       c'
            , "POST" :: ResourceMethod
                        pr
                        e
                        { path :: Path, method :: Method | req }
                        req'
                        res
                        res'
                        c
                        c'
            }
         -> PartialMiddleware 
            e 
            { path :: Path, method :: Method | req }
            req'
            res 
            res' 
            c
            c'
resource r conn =
  if r.path == conn.request.path
  then case handler of
    Just mw -> Just <$> mw conn
    Nothing -> pure Nothing
  else pure Nothing
  where
    handler =
      case conn.request.method of
        GET -> methodHandler r."GET"
        POST -> methodHandler r."POST"
