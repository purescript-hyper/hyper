module Hyper.Router ( Path
                    , pathToHtml
                    , pathFromString
                    , Supported
                    , Unsupported
                    , ResourceMethod
                    , handler
                    , notSupported
                    , resource
                    , ResourceRecord
                    , ResourceRouter()
                    , fallbackTo
                    ) where

import Prelude
import Control.Alt (class Alt)
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(MaybeT))
import Data.Array (filter)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), split, joinWith)
import Hyper.Conn (Conn)
import Hyper.Method (Method(POST, GET))
import Hyper.Middleware (Middleware)

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

handler :: forall e req req' res res' c c'.
           Middleware e (Conn req res c) (Conn req' res' c')
           -> ResourceMethod Supported e (Conn req res c) (Conn req' res' c')
handler mw = Routed mw Supported id

notSupported :: forall e req res c req' res' c'.
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

newtype ResourceRouter m c c' =
  ResourceRouter (Middleware (MaybeT m) c c')

instance functorResourceRouter :: Functor m => Functor (ResourceRouter m c) where
  map f (ResourceRouter r) = ResourceRouter $ \conn -> f <$> (r conn)

instance altResourceRouter :: Monad m => Alt (ResourceRouter m c) where
  -- We only want to run 'g' if 'f' resulted in a 'Nothing'.
  alt (ResourceRouter f) (ResourceRouter g) = ResourceRouter $ \conn -> MaybeT do
    result <- runMaybeT (f conn)
    case result of
      Just conn' -> pure (Just conn')
      Nothing -> runMaybeT (g conn)

type ResourceRecord m gr pr c c' = 
  { path :: Path
  , "GET" :: ResourceMethod gr m c c'
  , "POST" :: ResourceMethod pr m c c'
  }

resource :: forall gr pr m req res c req' res' c'.
            Applicative m =>
            ResourceRecord
            m
            gr
            pr
            (Conn { path :: Path, method :: Method | req } res c)
            (Conn { path :: Path, method :: Method | req' } res' c')
         -> ResourceRouter 
            m
            (Conn { path :: Path, method :: Method | req } res c)
            (Conn { path :: Path, method :: Method | req' } res' c')
resource r =
  ResourceRouter $ MaybeT <$> result
  where
    handler' conn =
      case conn.request.method of
        GET -> methodHandler r."GET"
        POST -> methodHandler r."POST"
    result conn =
      if r.path == conn.request.path
      then case handler' conn of
        Just mw -> Just <$> mw conn
        Nothing -> pure Nothing
      else pure Nothing

fallbackTo :: forall m req req' res res' c c'.
              Monad m =>
              Middleware m (Conn req res c) (Conn req' res' c')
              -> ResourceRouter m (Conn req res c) (Conn req' res' c')
              -> Middleware m (Conn req res c) (Conn req' res' c')
fallbackTo fallback (ResourceRouter rr) conn = do
  result <- runMaybeT $ rr conn
  case result of
    Just conn' -> pure conn'
    Nothing -> fallback conn
