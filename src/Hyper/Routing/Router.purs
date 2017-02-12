module Hyper.Routing.Router
       ( RoutingError(..)
       , class Router
       , route
       , router
       ) where

import Prelude
import Data.HTTP.Method as Method
import Data.StrMap as StrMap
import Control.IxMonad (ibind)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (elem, filter, null, uncons)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (textPlain)
import Data.StrMap (StrMap)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Hyper.Conn (Conn)
import Hyper.ContentNegotiation (AcceptHeader, acceptAll, negotiateContent, parseAcceptHeader)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Response (class Response, contentType, respond, class ResponseWriter, ResponseEnded, StatusLineOpen, closeHeaders, end, writeStatus)
import Hyper.Routing (type (:>), type (:<|>), Capture, CaptureAll, Handler, Lit, Raw, (:<|>))
import Hyper.Routing.ContentType (class AllMimeRender, allMimeRender)
import Hyper.Routing.PathPiece (class FromPathPiece, fromPathPiece)
import Hyper.Status (Status, statusBadRequest, statusMethodNotAllowed, statusNotAcceptable, statusNotFound, statusOK)
import Type.Proxy (Proxy(..))

type Method' = Either Method CustomMethod

type RoutingContext = { path :: Array String
                      , method :: Method'
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


instance routerLit :: ( Router e h out
                      , IsSymbol lit
                      )
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

instance routerCapture :: ( Router e h out
                          , FromPathPiece v
                          )
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


instance routerCaptureAll :: ( Router e h out
                             , FromPathPiece v
                             )
                             => Router (CaptureAll c v :> e) (Array v -> h) out where
  route _ ctx r =
    case traverse fromPathPiece ctx.path of
      Left err -> throwError (HTTPError { status: statusBadRequest
                                        , message: Just err
                                        })
      Right xs -> route (Proxy :: Proxy e) ctx { path = [] } (r xs)

routeEndpoint :: forall e r method.
                 (IsSymbol method)
                 => Proxy e
                 -> RoutingContext
                 -> r
                 -> SProxy method
                 -> Either RoutingError r
routeEndpoint _ context r methodProxy = do
  unless (null context.path) $
    throwError (HTTPError { status: statusNotFound
                          , message: Nothing
                          })

  let expectedMethod = Method.fromString (reflectSymbol methodProxy)
  unless (expectedMethod == context.method) $
    throwError (HTTPError { status: statusMethodNotAllowed
                          , message: Just ("Method "
                                           <> show context.method
                                           <> " did not match "
                                           <> show expectedMethod
                                           <> ".")
                          })
  pure r

getAccept :: StrMap String -> Either String (Maybe AcceptHeader)
getAccept m =
  case StrMap.lookup "accept" m of
    Just a -> Just <$> parseAcceptHeader a
    Nothing -> pure Nothing

instance routerHandler :: ( Monad m
                          , ResponseWriter rw m wb
                          , Response wb m r
                          , IsSymbol method
                          , AllMimeRender body ct r
                          )
                       => Router
                          (Handler method ct body)
                          (ExceptT RoutingError m body)
                          (Middleware
                           m
                           { request :: { method :: Either Method CustomMethod, url :: String, headers :: StrMap String | req }
                           , response :: { writer :: rw StatusLineOpen | res }
                           , components :: c
                           }
                           { request :: { method :: Either Method CustomMethod, url :: String, headers :: StrMap String | req }
                           , response :: { writer :: rw ResponseEnded | res }
                           , components :: c
                           }
                           Unit)
  where
  route proxy context action = do
    let handler = lift' (runExceptT action) `ibind`
                  case _ of
                    Left err -> do
                      writeStatus statusBadRequest
                      contentType textPlain
                      closeHeaders
                      end
                    Right body -> do
                      conn ← getConn
                      case getAccept conn.request.headers of
                        Left err -> do
                          writeStatus statusBadRequest
                          contentType textPlain
                          closeHeaders
                          end
                        Right parsedAccept -> do
                          case negotiateContent (fromMaybe acceptAll parsedAccept) (allMimeRender (Proxy :: Proxy ct) body) of
                            Just (Tuple ct rendered) -> do
                              writeStatus statusOK
                              contentType ct
                              closeHeaders
                              respond rendered
                            Nothing -> do
                              writeStatus statusNotAcceptable
                              contentType textPlain
                              closeHeaders
                              end
    routeEndpoint proxy context handler (SProxy :: SProxy method)
    where bind = ibind

instance routerRaw :: (IsSymbol method)
                   => Router
                      (Raw method)
                      (Middleware
                       m
                       { request :: { method :: Either Method CustomMethod, url :: String | req }
                       , response :: { writer :: rw StatusLineOpen | res }
                       , components :: c
                       }
                       { request :: { method :: Either Method CustomMethod, url :: String | req }
                       , response :: { writer :: rw ResponseEnded | res }
                       , components :: c
                       }
                       Unit)
                      (Middleware
                       m
                       { request :: { method :: Either Method CustomMethod, url :: String | req }
                       , response :: { writer :: rw StatusLineOpen | res }
                       , components :: c
                       }
                       { request :: { method :: Either Method CustomMethod, url :: String | req }
                       , response :: { writer :: rw ResponseEnded | res }
                       , components :: c
                       }
                       Unit)
                      where
  route proxy context r =
    routeEndpoint proxy context r (SProxy :: SProxy method)


router
  :: forall s r m req res c rw.
     ( Monad m
     , Router s r (Middleware
                   m
                   (Conn { method :: Method', url :: String | req } { writer :: rw StatusLineOpen | res } c)
                   (Conn { method :: Method', url :: String | req } { writer :: rw ResponseEnded | res } c)
                   Unit)
     ) =>
     Proxy s
  -> r
  -> (Status
      -> Maybe String
      -> Middleware
         m
         (Conn { method :: Method', url :: String | req } { writer :: rw StatusLineOpen | res } c)
         (Conn { method :: Method', url :: String | req } { writer :: rw ResponseEnded | res } c)
         Unit)
  -> Middleware
     m
     (Conn { method :: Method', url :: String | req } { writer :: rw StatusLineOpen | res } c)
     (Conn { method :: Method', url :: String | req } { writer :: rw ResponseEnded | res } c)
     Unit

router site handler onRoutingError = do
  handler'
  -- Run the routing to get a handler.
  -- route (Proxy :: Proxy s) ctx handler
  -- Then, if successful, run the handler, possibly also generating an HTTPError.
  -- # either catch runHandler
  where
    splitUrl = filter ((/=) "") <<< split (Pattern "/")
    context conn = { path: splitUrl conn.request.url
                   , method: conn.request.method
                   }
    catch (HTTPError { status, message }) =
      onRoutingError status message

    handler' ∷ Middleware
               m
               (Conn { method :: Method', url :: String | req } { writer :: rw StatusLineOpen | res } c)
               (Conn { method :: Method', url :: String | req } { writer :: rw ResponseEnded | res } c)
               Unit
    handler' = do
      conn ← getConn
      case route site (context conn) handler of
        Left err → catch err
        Right h → h

    bind = ibind
