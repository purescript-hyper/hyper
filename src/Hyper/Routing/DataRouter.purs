module Hyper.Routing.DataRouter
       ( GET
       , POST
       , Path
       , Route(..)
       , class Addressable
       , toPath
       , class Routable
       , fromRoute
       , router
       , linkTo
       , redirectTo
       , formFor
       ) where

import Prelude
import Hyper.Method as Method
import Data.Array (filter)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (joinWith, Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (class ResponseWriter, statusFound, end, writeStatus, ResponseEnded, StatusLineOpen, TryMiddleware(TryMiddleware), Conn, Middleware)
import Hyper.HTML (a, form, HTML)
import Hyper.Method (Method)
import Hyper.Response (headers)

data GET
data POST

type Path = Array String

pathToHtml :: Path -> String
pathToHtml = (<>) "/" <<< joinWith "/"

pathFromString :: String -> Path
pathFromString = filter ((/=) "") <<< split (Pattern "/")

data Route m = Route Path

class Addressable r where
  toPath :: forall m. r m -> Path

class Routable r m where
  fromRoute :: Route m -> Maybe (r m)

type MethodRequestHandler r method m req res c req' res' c' =
  r method -> Middleware
              m
              (Conn { method :: Method, url :: String | req } res c)
              (Conn req' res' c')

routeWith :: forall c r method m c'.
  ( Routable r method , Functor m , Applicative m ) =>
  c -> Route method -> (r method -> c -> m c') -> m (Maybe c')
routeWith conn route handler =
  case fromRoute route of
    Just requestable ->
      Just <$> handler requestable conn
    Nothing -> pure Nothing

router
  :: forall m r req res c req' res' c'.
     (Applicative m, Routable r GET, Routable r POST) =>
  { get :: MethodRequestHandler r GET m req res c req' res' c'
  , post :: MethodRequestHandler r POST m req res c req' res' c'
  }
  -> TryMiddleware m
                   (Conn { method :: Method, url :: String | req } res c)
                   (Conn req' res' c')
router handlers = TryMiddleware router'
  where
    router' conn =
      let path = pathFromString conn.request.url
      in case conn.request.method of
        Method.GET -> routeWith conn (Route path) handlers.get
        Method.POST -> routeWith conn (Route path) handlers.post

type WithMethod m r = (m ~ m) -> r m

linkTo
  :: forall r. Addressable r =>
     WithMethod GET r
  -> Array HTML
  -> HTML
linkTo r children =
  a [Tuple "href" (pathToHtml (toPath (r id)))] children

redirectTo
  :: forall m r req res c rw b.
     (Monad m, Addressable r, ResponseWriter rw m b) =>
     WithMethod GET r
  -> Middleware
     m
     (Conn req { writer :: rw StatusLineOpen | res } c)
     (Conn req { writer :: rw ResponseEnded | res } c)
redirectTo r =
  writeStatus statusFound
  >=> headers [Tuple "Location" (pathToHtml (toPath (r id)))]
  >=> end

formFor
  :: forall r. (Addressable r) =>
     WithMethod POST r
  -> Array HTML
  -> HTML
formFor r children =
  form
  [ Tuple "method" "POST"
  , Tuple "action" (pathToHtml (toPath (r id)))
  ]
  children
