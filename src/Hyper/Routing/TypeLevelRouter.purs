-- Highly experimental (and naive) implementation of servant-server style
-- routing for Hyper.
module Hyper.Routing.TypeLevelRouter
       ( Lit
       , Capture
       , CaptureAll
       , Handler
       , Get
       , Sub
       , LitSub
       , AltE(..)
       , type (:>)
       , type (:/)
       , type (:<|>)
       , (:<|>)
       , Link
       , class HasLinks
       , toLinks
       , linksTo
       , RoutingError(..)
       , class Router
       , route
       , router
       ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array (elem, filter, foldl, null, singleton, uncons, unsnoc)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Path.Pathy (dir, file, rootDir, (</>))
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.URI (HierarchicalPart(..), URI(..))
import Hyper.Core (class ResponseWriter, Conn, Middleware, ResponseEnded, Status, StatusLineOpen, TryMiddleware(..), closeHeaders, statusBadRequest, statusMethodNotAllowed, statusNotFound, writeStatus)
import Hyper.Method (Method)
import Hyper.Response (class Response, respond)
import Hyper.Routing.PathPiece (class FromPathPiece, class ToPathPiece, fromPathPiece, toPathPiece)
import Type.Proxy (Proxy(..))

-- | A literal path segment, matching paths where the next segment is equal
-- | to the value of the `Symbol`.
-- |
-- | For example, the type `Lit "settings" :> Lit "general" :> "reset"`  would
-- | match the path `/settings/general/reset`.
data Lit (v :: Symbol)

-- | Captures one segment of a path as type `t`. The `v` is a
-- | `Symbol` that describes the captured value.
data Capture (v :: Symbol) t

-- | Captures all remaining segments of a path, all as type `t`. The `v`
-- | is a `Symbol` that describes
data CaptureAll (v :: Symbol) t

-- | A type-level description of the handler function, terminating a chain of
-- | path literals, captures, and other endpoint type constructs. The `m` symbol
-- | is the HTTP method that is handled.
data Handler (m :: Symbol)

-- | Handy alias for GET handlers.
type Get = Handler "GET"

-- | The `Sub` is used to create the chain of `Lit`, `Capture`, `Handler`,
-- | and other such type constructs that build up an endpoint type. `Sub`
-- | is more often used infix with the `:>` operator.
data Sub e t

-- | A handy type alias for `Sub (Lit v)`, meant to be used infix with the `:/`
-- | operator. Instead of writing `Lit "a" :> Lit "b" :> ...`, you can write
-- | `"a" :/ "b" :/ ...`.
type LitSub (v :: Symbol) t = Sub (Lit v) t

-- | `AltE`` respresents choice, i.e. that endpoint `a` is tried first, and if
-- | it fails, `b` is tried next. `AltE` is written infix using `:<|>` and is
-- | used to compose multiple endpoint types into a larger API or site. It is
-- | used to build up recursive structures, so `AltE a (AltE b c)` can be
-- | written `a :<|> b :<|> c`.
-- |
-- | It it also used to extract information from a type, where the information
-- | has the same structure as the type. For instance, when extracting links
-- | from an `AltE` type, you can pattern match the result using `:<|>`
-- | to access the links of `a` and `b`. That also works recursively with a
-- | pattern match like `a :<|> b :<|> c :<|> d`.
data AltE a b = AltE a b

infixr 5 type Sub as :>
infixr 5 type LitSub as :/
infixl 4 type AltE as :<|>
infixl 4 AltE as :<|>

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
  (HierarchicalPart Nothing (Just path))
  Nothing
  Nothing
  where
    path =
      case unsnoc segments of
        Just { init, last } ->
          Right (foldl (</>) rootDir (map dir init) </> file last)
        Nothing ->
          Left rootDir

class HasLinks e mk | e -> mk where
  toLinks :: Proxy e -> Link -> mk

instance hasLinksLit :: (HasLinks sub subMk, IsSymbol lit)
                       => HasLinks (Lit lit :> sub) subMk where
  toLinks _ =
    toLinks (Proxy :: Proxy sub) <<< flip append (Link [segment])
    where
      segment = reflectSymbol (SProxy :: SProxy lit)

instance hasLinksCapture :: (HasLinks sub subMk, IsSymbol c, ToPathPiece t)
                           => HasLinks (Capture c t :> sub) (t -> subMk) where
  toLinks _ l =
    toLinks (Proxy :: Proxy sub) <<< append l <<< Link <<< singleton <<< toPathPiece

instance hasLinksCaptureAll :: (HasLinks sub subMk, IsSymbol c, ToPathPiece t)
                              => HasLinks (CaptureAll c t :> sub) (Array t -> subMk) where
  toLinks _ l =
    toLinks (Proxy :: Proxy sub) <<< append l <<< Link <<< map toPathPiece

instance hasLinksHandler :: HasLinks (Handler m) URI where
  toLinks _ = linkToURI

instance hasLinksAltE :: (HasLinks e1 mk1, HasLinks e2 mk2) => HasLinks (e1 :<|> e2) (mk1 :<|> mk2) where
  toLinks _ link =
    toLinks (Proxy :: Proxy e1) link
    :<|> toLinks (Proxy :: Proxy e2) link

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

instance routerAltE :: (Router e1 h1 out, Router e2 h2 out)
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

instance routerCapture :: (Router e h out, FromPathPiece v)
                          => Router (Capture c v :> e) (v -> h) out where
  route _ ctx r =
    case uncons ctx.path of
      Nothing -> throwError (HTTPError statusNotFound Nothing)
      Just { head, tail } ->
        case fromPathPiece head of
          Left err -> throwError (HTTPError statusBadRequest (Just err))
          Right x -> route (Proxy :: Proxy e) ctx { path = tail } (r x)

instance routerCaptureAll :: (Router e h out, FromPathPiece v)
                             => Router (CaptureAll c v :> e) (Array v -> h) out where
  route _ ctx r =
    case traverse fromPathPiece ctx.path of
      Left err -> throwError (HTTPError statusBadRequest (Just err))
      Right xs -> route (Proxy :: Proxy e) ctx { path = [] } (r xs)

instance routerHandler :: (IsSymbol m)
                       => Router (Handler m) h h where
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
