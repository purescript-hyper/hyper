module Hyper.Router where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn(Conn), ResponseMiddleware, Middleware)
import Hyper.Method (Method)
import Hyper.Stream (Initial, Stream)

-- TODO: Make Path an Array of segments
type Path = String

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
router routeFn (Conn c) = do
  let pathComponent = fromPath (Route c.request.method c.request.path)
  conn <- _addRoutes (Conn c)
  routeFn pathComponent conn
