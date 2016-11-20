module Hyper.Router where

import Prelude
import Data.Array (filter)
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

data MethodHandler e req req' res res' c c'
  = Routed (Middleware e req req' res res' c c')
  | NotRouted

data ResourceMethod r e req req' res res' c c'
  = ResourceMethod r (MethodHandler e req req' res res' c c')

class MethodRouter mr e req req' res res' c c' where
  routeMethod :: mr -> Maybe (Middleware e req req' res res' c c')

instance methodRouterRouted :: MethodRouter
                               (ResourceMethod m  e req req' res res' c c')
                               e req req' res res' c c' where
  routeMethod (ResourceMethod _ (Routed mw)) = Just mw
  routeMethod (ResourceMethod _ NotRouted) = Nothing

foreign import _router :: forall ms e req req' res res' c c'.
                          { path :: Path | ms }
                       -> String
                       -> Middleware e req req' res res' c c'

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
  then case method of
    Just mw -> Just <$> mw conn
    Nothing -> pure Nothing
  else pure Nothing
  where
    method =
      case conn.request.method of
        GET -> routeMethod r."GET"
        POST -> routeMethod r."POST"
