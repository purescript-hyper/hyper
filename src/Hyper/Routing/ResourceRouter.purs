module Hyper.Routing.ResourceRouter ( Path
                                    , pathToHtml
                                    , pathFromString
                                    , Supported
                                    , Unsupported
                                    , ResourceMethod
                                    , handler
                                    , notSupported
                                    , resource
                                    , ResourceRecord
                                    , router
                                    , ResourceRouter()
                                    , runRouter
                                    , defaultRouterFallbacks
                                    , linkTo
                                    , formTo
                                    ) where

import Prelude
import Control.Alt (class Alt)
import Data.Array (filter)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), split, joinWith)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (class ResponseWriter, ResponseEnded, StatusLineOpen, statusNotFound, writeStatus, statusMethodNotAllowed, Middleware, Conn)
import Hyper.HTML (form, a, HTML)
import Hyper.Method (Method(POST, GET))
import Hyper.Response (class Response, respond, headers)

type Path = Array String

pathToHtml :: Path -> String
pathToHtml = (<>) "/" <<< joinWith "/"

pathFromString :: String -> Path
pathFromString = filter ((/=) "") <<< split (Pattern "/")

data Supported = Supported
data Unsupported = Unsupported

data ResourceMethod r m x y
  = Routed (Middleware m x y) r (r ~ Supported)
  | NotRouted r (r ~ Unsupported)

handler :: forall m req req' res res' c c'.
           Middleware m (Conn req res c) (Conn req' res' c')
           -> ResourceMethod Supported m (Conn req res c) (Conn req' res' c')
handler mw = Routed mw Supported id

notSupported :: forall e req res c req' res' c'.
                ResourceMethod Unsupported e (Conn req res c) (Conn req' res' c')
notSupported = NotRouted Unsupported id

methodHandler :: forall m e x y.
                 ResourceMethod m e x y
                 -> Maybe (Middleware e x y)
methodHandler (Routed mw _ _) = Just mw
methodHandler (NotRouted _ _) = Nothing

data RoutingResult c
  = RouteMatch c
  | NotAllowed Method
  | NotFound

instance functorRoutingResult :: Functor RoutingResult where
  map f =
    case _ of
      RouteMatch c -> RouteMatch (f c)
      NotAllowed method -> NotAllowed method
      NotFound -> NotFound

newtype ResourceRouter m c c' = ResourceRouter (Middleware m c (RoutingResult c'))

instance functorResourceRouter :: Functor m => Functor (ResourceRouter m c) where
  map f (ResourceRouter r) = ResourceRouter $ \conn -> map (map f) (r conn)

instance altResourceRouter :: Monad m => Alt (ResourceRouter m c) where
  -- NOTE: We have strict evaluation, and we only want to run 'g' if 'f'
  -- resulted in a `NotFound`.
  alt (ResourceRouter f) (ResourceRouter g) = ResourceRouter $ \conn -> do
    result <- f conn
    case result of
      RouteMatch conn' -> pure (RouteMatch conn')
      NotAllowed method -> pure (NotAllowed method)
      NotFound -> g conn

type ResourceRecord m gr pr c c' =
  { path :: Path
  , "GET" :: ResourceMethod gr m c c'
  , "POST" :: ResourceMethod pr m c c'
  }

resource
  :: forall m req res c req' res' c'.
     { path :: Unit
     , "GET" :: ResourceMethod Unsupported m (Conn req res c) (Conn req' res' c')
     , "POST" :: ResourceMethod Unsupported m (Conn req res c) (Conn req' res' c')
     }
resource =
  { path: unit
  , "GET": notSupported
  , "POST": notSupported
  }

router
  :: forall gr pr m req res c req' res' c'.
     Applicative m =>
     ResourceRecord
     m
     gr
     pr
     (Conn { url :: String, method :: Method | req } res c)
     (Conn { url :: String, method :: Method | req' } res' c')
  -> ResourceRouter
     m
     (Conn { url :: String, method :: Method | req } res c)
     (Conn { url :: String, method :: Method | req' } res' c')
router r =
  ResourceRouter result
  where
    handler' conn =
      case conn.request.method of
        GET -> methodHandler r."GET"
        POST -> methodHandler r."POST"
    result conn =
      if r.path == pathFromString conn.request.url
      then case handler' conn of
        Just mw -> RouteMatch <$> mw conn
        Nothing -> pure (NotAllowed conn.request.method)
      else pure NotFound

type RouterFallbacks m c c' =
  { onNotFound :: Middleware m c c'
  , onMethodNotAllowed :: Method -> Middleware m c c'
  }

defaultRouterFallbacks
  :: forall m rw b req res c.
     (Monad m, Response m String b, ResponseWriter rw b m) =>
     RouterFallbacks
     m
     (Conn req { writer :: rw StatusLineOpen | res } c)
     (Conn req { writer :: rw ResponseEnded | res } c)
defaultRouterFallbacks =
  { onNotFound:
    writeStatus statusNotFound
    >=> headers []
    >=> respond "Not Found"
  , onMethodNotAllowed:
    \method ->
    writeStatus statusMethodNotAllowed
    >=> headers []
    >=> respond ("Method " <> show method <> " not allowed.")
  }

runRouter
  :: forall m c c'.
     Monad m =>
     RouterFallbacks m c c'
     -> ResourceRouter m c c'
     -> Middleware m c c'
runRouter fallbacks (ResourceRouter rr) conn = do
  result <- rr conn
  case result of
    RouteMatch conn' -> pure conn'
    NotAllowed method -> fallbacks.onMethodNotAllowed method conn
    NotFound -> fallbacks.onNotFound conn

linkTo :: forall m c c' ms.
          { path :: Path
          , "GET" :: ResourceMethod Supported m c c'
          | ms }
          -> Array HTML
          -> HTML
linkTo resource' nested = do
  a [Tuple "href" (pathToHtml resource'.path)] nested

formTo :: forall m c c' ms.
          { path :: Path
          , "POST" :: ResourceMethod Supported m c c'
          | ms
          }
          -> Array HTML
          -> HTML
formTo resource' nested =
  form
  [ Tuple "method" "post"
  , Tuple "action" (pathToHtml resource'.path)
  ]
  nested
