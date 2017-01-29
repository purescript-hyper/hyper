-- Highly experimental (and naive) implementation of servant-server style
-- routing for Hyper.
module Hyper.Routing.TypeLevelRouter
       ( Lit
       , Capture
       , CaptureAll
       , Handler
       , Raw
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
       , RawHandler(Raw)
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
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Path.Pathy (dir, file, rootDir, (</>))
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.URI (HierarchicalPart(..), URI(..))
import Hyper.Core (class ResponseWriter, Conn, Middleware, ResponseEnded, StatusLineOpen, closeHeaders, writeStatus)
import Hyper.Method (Method)
import Hyper.Response (class Response, contentType, respond)
import Hyper.Routing.ContentType (class HasMediaType, class MimeRender, getMediaType, mimeRender)
import Hyper.Routing.PathPiece (class FromPathPiece, class ToPathPiece, fromPathPiece, toPathPiece)
import Hyper.Status (Status, statusBadRequest, statusMethodNotAllowed, statusNotFound, statusOK)
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
-- | is the HTTP method that is handled. `ct` is the content type.
data Handler (m :: Symbol) ct b

-- | A type-level description of a raw handler middleware, terminating a chain
-- | of path literals, captures, and other endpoint type constructs. The `m`
-- | symbol is the HTTP method that is handled.
data Raw (m :: Symbol)

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

instance hasLinksHandler :: HasLinks (Handler m ct b) URI where
  toLinks _ = linkToURI

instance hasLinksRaw :: HasLinks (Raw m) URI where
  toLinks _ = linkToURI

instance hasLinksAltE :: (HasLinks e1 mk1, HasLinks e2 mk2) => HasLinks (e1 :<|> e2) (mk1 :<|> mk2) where
  toLinks _ link =
    toLinks (Proxy :: Proxy e1) link
    :<|> toLinks (Proxy :: Proxy e2) link

linksTo :: forall e t. HasLinks e t => Proxy e -> t
linksTo e = toLinks e mempty

type RoutingContext = { path :: Array String
                      , method :: Method
                      }

data RoutingError
  = HTTPError { status :: Status
              , message :: Maybe String
              }

derive instance genericRoutingError :: Generic RoutingError _

instance eqRoutingError :: Eq RoutingError where
  eq = genericEq

instance showRoutingError :: Show RoutingError where
  show = genericShow

class Router e h r | e -> h, e -> r where
  route :: Proxy e -> RoutingContext -> h -> Either RoutingError r

instance routerAltE :: (Router e1 h1 out, Router e2 h2 out)
                            => Router (e1 :<|> e2) (h1 :<|> h2) out where
  route _ context (h1 :<|> h2) =
    case route (Proxy :: Proxy e1) context h1 of
      Left err1 ->
        case route (Proxy :: Proxy e2) context h2 of
          -- The Error that's thrown depends on the Errors' HTTP codes.
          Left err2 -> throwError (selectError err1 err2)
          Right handler -> pure handler
      Right handler -> pure handler
    where
      fallbackStatuses = [statusNotFound, statusMethodNotAllowed]
      selectError (HTTPError errL) (HTTPError errR) =
        case Tuple errL.status errR.status of
          Tuple  s1 s2
            | s1 `elem` fallbackStatuses && s2 == statusNotFound -> HTTPError errL
            | s1 /= statusNotFound && s2 `elem` fallbackStatuses -> HTTPError errL
            | otherwise -> HTTPError errR


instance routerLit :: (Router e h out, IsSymbol lit)
                      => Router (Lit lit :> e) h out where
  route _ ctx r =
    case uncons ctx.path of
      Just { head, tail } | head == expectedSegment ->
        route (Proxy :: Proxy e) ctx { path = tail} r
      Just _ -> throwError (HTTPError { status: statusNotFound
                                      , message: Nothing
                                      })
      Nothing -> throwError (HTTPError { status: statusNotFound
                                       , message: Nothing
                                       })
    where expectedSegment = reflectSymbol (SProxy :: SProxy lit)

instance routerCapture :: (Router e h out, FromPathPiece v)
                          => Router (Capture c v :> e) (v -> h) out where
  route _ ctx r =
    case uncons ctx.path of
      Nothing -> throwError (HTTPError { status: statusNotFound
                                       , message: Nothing
                                       })
      Just { head, tail } ->
        case fromPathPiece head of
          Left err -> throwError (HTTPError { status: statusBadRequest
                                            , message: Just err
                                            })
          Right x -> route (Proxy :: Proxy e) ctx { path = tail } (r x)


instance routerCaptureAll :: (Router e h out, FromPathPiece v)
                             => Router (CaptureAll c v :> e) (Array v -> h) out where
  route _ ctx r =
    case traverse fromPathPiece ctx.path of
      Left err -> throwError (HTTPError { status: statusBadRequest
                                        , message: Just err
                                        })
      Right xs -> route (Proxy :: Proxy e) ctx { path = [] } (r xs)

routeEndpoint :: forall e r m. (IsSymbol m)
                 => Proxy e
                 -> RoutingContext
                 -> r
                 -> SProxy m
                 -> Either RoutingError r
routeEndpoint _ context r methodProxy = do
  unless (null context.path) $
    throwError (HTTPError { status: statusNotFound
                          , message: Nothing
                          })

  let expectedMethod = reflectSymbol methodProxy
  unless (expectedMethod == show context.method) $
    throwError (HTTPError { status: statusMethodNotAllowed
                          , message: Just ("Method "
                                           <> show context.method
                                           <> " did not match "
                                           <> expectedMethod
                                           <> ".")
                          })
  pure r

newtype RawHandler m req res c rw =
  Raw
  (Middleware
   m
   (Conn { method :: Method, url :: String | req } { writer :: rw StatusLineOpen | res } c)
   (Conn { method :: Method, url :: String | req } { writer :: rw ResponseEnded | res } c))

instance routerHandler :: ( Monad m
                          , ResponseWriter rw m wb
                          , Response wb m r
                          , IsSymbol method
                          , MimeRender body ct r
                          , HasMediaType ct
                          )
                       => Router
                          (Handler method ct body)
                          (m body)
                          (RawHandler m req res c rw) where
  route proxy context action = do
    let handler conn = do
          body <- action
          writeStatus statusOK conn
            >>= contentType (getMediaType (Proxy :: Proxy ct))
            >>= closeHeaders
            >>= respond (mimeRender (Proxy :: Proxy ct) body)
    routeEndpoint proxy context (Raw handler) (SProxy :: SProxy method)

instance routerRaw :: (IsSymbol method)
                       => Router (Raw method) (RawHandler m req res c rw) (RawHandler m req res c rw) where
  route proxy context r =
    routeEndpoint proxy context r (SProxy :: SProxy method)

router
  :: forall s r m req res c rw wb.
     ( Monad m
     , ResponseWriter rw m wb
     , Router s r (RawHandler m req res c rw)
     ) =>
     Proxy s
  -> r
  -> (Status
      -> Maybe String
      -> Middleware
         m
         (Conn { method :: Method, url :: String | req } { writer :: rw StatusLineOpen | res } c)
         (Conn { method :: Method, url :: String | req } { writer :: rw ResponseEnded | res } c))
  -> Middleware
     m
     (Conn { method :: Method, url :: String | req } { writer :: rw StatusLineOpen | res } c)
     (Conn { method :: Method, url :: String | req } { writer :: rw ResponseEnded | res } c)
router _ handler onRoutingError conn =
  case route (Proxy :: Proxy s) { path: splitUrl conn.request.url
                                , method: conn.request.method
                                } handler of
    Left (HTTPError { status, message }) ->
      onRoutingError status message conn
    Right (Raw h) ->
      h conn
  where
    splitUrl = filter ((/=) "") <<< split (Pattern "/")
