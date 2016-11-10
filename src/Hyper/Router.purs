module Hyper.Router where

import Prelude
import Data.Array (filter)
import Data.String (Pattern(Pattern), split, joinWith)
import Hyper.Conn (Middleware)
import Hyper.Method (Method)

type Path = Array String

pathToHtml :: Path -> String
pathToHtml = (<>) "/" <<< joinWith "/"

pathFromString :: String -> Path
pathFromString = filter ((/=) "") <<< split (Pattern "/")

data Route = Route Method Path

class Routable p where
  fromPath :: Route -> p
  toPath :: p -> Route

type RouterFn r e req req' res res' c c' =
  Routable r => r
  -> Middleware e req req' res res' c c'

foreign import _addRoutes :: forall r e req res c. Routable r =>
                             Middleware e req req res res  { | c } { routes :: r | c }

router :: forall r e req req' res res' c.
          Routable r =>
          RouterFn
          r
          e
          { path :: String, method :: Method | req }
          { path :: String, method :: Method | req' }
          res
          res'
          { routes :: r | c}
          { routes :: r | c}
          -> Middleware
             e
             { path :: String, method :: Method | req }
             { path :: String, method :: Method | req' }
             res
             res'
             { | c }
             { routes :: r | c }
router routeFn c = do
  let path = pathFromString c.request.path
      pathComponent = fromPath (Route c.request.method path)
  c' <- _addRoutes c
  routeFn pathComponent c'
