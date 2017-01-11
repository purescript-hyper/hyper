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
       , formFor
       ) where

import Prelude
import Hyper.Method as Method
import Data.Array (filter)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (joinWith, Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (TryMiddleware(TryMiddleware), Conn, Middleware)
import Hyper.HTML (element, a, form, HTML)
import Hyper.Method (Method)

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
  case toPath (r id) of
    path ->
      a [Tuple "href" (pathToHtml path)] children

formFor
  :: forall r a. (Addressable r, Show a) =>
     (a -> WithMethod POST r)
  -> a
  -> HTML
formFor r value =
  case toPath (r value id) of
    path ->
      form
      [ Tuple "method" "POST"
      , Tuple "action" (pathToHtml path)
      ]
      -- Dummy implementation for now...
      [ element
        "input"
        [ Tuple "name" "value"
        , Tuple "value" (show value)
        ]
        []
      ]
