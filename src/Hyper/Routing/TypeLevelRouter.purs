-- Highly experimental and naive implementation of servant-server style
-- routing for Hyper. Not much will be stable here.
module Hyper.Routing.TypeLevelRouter
       ( Lit
       , Capture
       , Verb
       , Get
       , Sub
       , LitSub
       , Endpoints(..)
       , type (:>)
       , type (:/)
       , type (:<|>)
       , (:<|>)
       , class ToHttpData
       , toPathPiece
       , class FromHttpData
       , fromPathPiece
       , Link
       , class HasLink
       , toLink
       , class HasLinks
       , toLinks
       , RoutingError(..)
       , class Router
       , route
       , router
       ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array (elem, filter, foldl, null, uncons)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Path.Pathy (dir, rootDir, (</>))
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.URI (HierarchicalPart(..), URI(..))
import Hyper.Core (class ResponseWriter, Conn, Middleware, ResponseEnded, Status, StatusLineOpen, TryMiddleware(..), closeHeaders, statusBadRequest, statusMethodNotAllowed, statusNotFound, writeStatus)
import Hyper.Method (Method)
import Hyper.Response (class Response, respond)
import Type.Proxy (Proxy(..))

data Lit (v :: Symbol)
data Capture (v :: Symbol) t

data Verb (m :: Symbol)

type Get = Verb "GET"

data Sub e t
type LitSub (v :: Symbol) t = Sub (Lit v) t

data Endpoints a b = Endpoints a b

infixr 5 type Sub as :>
infixr 5 type LitSub as :/
infixl 4 type Endpoints as :<|>
infixl 4 Endpoints as :<|>

newtype Link = Link (Array String)

instance monoidLink :: Monoid Link where
  mempty = Link []

instance semigroupLink :: Semigroup Link where
  append (Link p1) (Link p2) = Link (p1 <> p2)

derive instance newtypeLink :: Newtype Link _

derive instance genericLink :: Generic Link _

instance eqLink :: Eq Link where
  eq = genericEq

linkToURI :: Link -> URI
linkToURI (Link segments) =
  URI
  Nothing
  (HierarchicalPart
   Nothing
   (Just (Left (foldl (</>) rootDir (map dir segments)))))
  Nothing
  Nothing

class ToHttpData x where
  toPathPiece :: x -> String

instance toHttpDataInt :: ToHttpData Int where
  toPathPiece = show

class FromHttpData x where
  fromPathPiece :: String -> Either String x

instance fromHttpDataInt :: FromHttpData Int where
  fromPathPiece s =
    case fromString s of
      Just n -> Right n
      Nothing -> Left ("Invalid Int: " <> s)

class HasLink e mk | e -> mk where
  toLink :: Proxy e -> Link -> mk

instance hasLinkLit :: (HasLink sub subMk, IsSymbol lit)
                       => HasLink (Lit lit :> sub) subMk where
  toLink _ =
    toLink (Proxy :: Proxy sub) <<< flip append (Link [segment])
    where
      segment = reflectSymbol (SProxy :: SProxy lit)

instance hasLinkCapture :: (HasLink sub subMk, IsSymbol c, ToHttpData t)
                           => HasLink (Capture c t :> sub) (t -> subMk) where
  toLink _ l (x :: t) =
    toLink (Proxy :: Proxy sub) $ append l (Link [toPathPiece x])

instance hasLinkVerb :: HasLink (Verb m) URI where
  toLink _ = linkToURI

linkTo :: forall l t. HasLink l t => Proxy l -> t
linkTo p = toLink p mempty

class HasLinks e mk | e -> mk where
  toLinks :: Proxy e -> Link -> mk

instance hasLinksEndpoints :: (HasLink e1 mk1, HasLink e2 mk2) => HasLinks (e1 :<|> e2) (mk1 :<|> mk2) where
  toLinks _ link =
    toLink (Proxy :: Proxy e1) link
    :<|> toLink (Proxy :: Proxy e2) link

linksTo :: forall e t. HasLinks e t => Proxy e -> t
linksTo e = toLinks e mempty

type RoutingContext = { path :: (Array String)
                      , method :: Method
                      }

data RoutingError
  = HTTPError Status (Maybe String)

derive instance genericRoutingError :: Generic RoutingError _

instance eqRoutingError :: Eq RoutingError where
  eq = genericEq

instance showRoutingError :: Show RoutingError where
  show = genericShow

class Router e h r | e -> h, e -> r where
  route :: Proxy e -> RoutingContext -> h -> Either RoutingError r

fallthrough :: RoutingError -> Boolean
fallthrough (HTTPError (Tuple code _) _) = code `elem` [404, 405]

instance routerEndpoints :: (Router e1 h1 out, Router e2 h2 out)
                            => Router (e1 :<|> e2) (h1 :<|> h2) out where
  route _ context (h1 :<|> h2) =
    case route (Proxy :: Proxy e1) context h1 of
      Left err ->
        if fallthrough err
        then route (Proxy :: Proxy e2) context h2
        else Left err
      Right handler -> pure handler

instance routerLit :: (Router e h out, IsSymbol lit)
                      => Router (Lit lit :> e) h out where
  route _ ctx r =
    case uncons ctx.path of
      Just { head, tail } | head == expectedSegment ->
        route (Proxy :: Proxy e) ctx { path = tail} r
      Just _ -> throwError (HTTPError statusNotFound Nothing)
      Nothing -> throwError (HTTPError statusNotFound Nothing)
    where expectedSegment = reflectSymbol (SProxy :: SProxy lit)

instance routerCapture :: (Router e h out, FromHttpData v)
                          => Router (Capture c v :> e) (v -> h) out where
  route _ ctx r =
    case uncons ctx.path of
      Nothing -> throwError (HTTPError statusNotFound Nothing)
      Just { head, tail } ->
        case fromPathPiece head of
          Left err -> throwError (HTTPError statusBadRequest (Just err))
          Right x -> route (Proxy :: Proxy e) ctx { path = tail } (r x)

instance routerVerb :: (IsSymbol m)
                       => Router (Verb m) h h where
  route _ context r =
    if expectedMethod == show context.method && null context.path
    then pure r
    else throwError (HTTPError
                     statusMethodNotAllowed
                     (Just ("Method "
                            <> show context.method
                            <> " did not match "
                            <> expectedMethod
                            <> ".")))
    where
      expectedMethod = reflectSymbol (SProxy :: SProxy m)

router
  :: forall s r m req res c rw b.
     ( Monad m
     , ResponseWriter rw m b
     , Response b m String
     , Router s r (Middleware
                   m
                   (Conn { method :: Method, url :: String | req } { writer :: rw StatusLineOpen | res } c)
                   (Conn { method :: Method, url :: String | req } { writer :: rw ResponseEnded | res } c))
     ) =>
     Proxy s
  -> r
  -> TryMiddleware
     m
     (Conn { method :: Method, url :: String | req } { writer :: rw StatusLineOpen | res } c)
     (Conn { method :: Method, url :: String | req } { writer :: rw ResponseEnded | res } c)
router _ handler = TryMiddleware router'
  where
    router' conn =
      case route (Proxy :: Proxy s) { path: splitUrl conn.request.url
                                    , method: conn.request.method
                                    } handler of
        Left (HTTPError (Tuple 404 _) _) ->
          pure Nothing
        Left (HTTPError status msg) ->
          writeStatus status conn
          >>= closeHeaders
          >>= respond (maybe "" id msg)
          # map Just
        Right h ->
          Just <$> h conn
    splitUrl = filter ((/=) "") <<< split (Pattern "/")
