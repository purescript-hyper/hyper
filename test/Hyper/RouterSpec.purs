module Hyper.RouterSpec where

import Prelude
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Writer (lift)
import Data.Newtype (unwrap)
import Hyper.Core (class ResponseWriter, Conn, HeadersOpen(..), Middleware, ResponseEnded)
import Hyper.Method (Method(..))
import Hyper.Response (headers, notFound, respond)
import Hyper.Router (Path, fallbackTo, handler, resource)
import Hyper.Test.TestServer (TestResponseWriter(..), testBody, testServer)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

app :: forall m req res rw c.
  (Monad m, ResponseWriter rw m) =>
  Middleware
  m
  (Conn { path :: Path, method :: Method | req }
        { writer :: rw, state :: HeadersOpen | res }
        c)
  (Conn { path :: Path, method :: Method | req }
        { writer :: rw, state :: ResponseEnded | res }
        c)
app = headers [] >=> (fallbackTo notFound $ resource greetings)
  where
    greetings =
      { path: []
      , "GET": handler (respond "Hello!")
      , "POST": handler (respond "OK, I've saved that for ya.")
      }

spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      response ←
            { request: { method: GET
                       , path: []
                       }
            , response: { state: HeadersOpen
                        , writer: TestResponseWriter
                        }
            , components: {}
            }
            # app
            # testServer
      testBody response `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      response ←
            { request: { method: POST
                       , path: []
                       }
            , response: { state: HeadersOpen
                        , writer: TestResponseWriter
                        }
            , components: {}
            }
            # app
            # testServer
      testBody response `shouldEqual` "OK, I've saved that for ya."
