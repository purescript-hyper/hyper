module Hyper.RouterSpec where

import Prelude
import Control.Monad.Eff.Console (CONSOLE)
import Hyper.Core (HeadersOpen(HeadersOpen))
import Hyper.Method (Method(..))
import Hyper.Response (headers, notFound, respond)
import Hyper.Router (fallbackTo, handler, resource)
import Hyper.TestUtilities (TestResponseWriter(TestResponseWriter))
import Test.Spec (Spec, it, describe)

spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  let greetings =
        { path: []
        , "GET": handler (respond "Hello!")
        , "POST": handler (respond "OK, I've saved that for ya.")
        }
      app = headers [] >=> (fallbackTo notFound $ resource greetings)

  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      conn <- app
              { request: { method: GET
                         , path: []
                         }
              , response: { state: HeadersOpen
                          , writer: TestResponseWriter
                          }
              , components: {}
              }
      pure unit
      -- conn.response.body `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      conn <- app
              { request: { method: POST
                         , path: []
                         }
              , response: { state: HeadersOpen
                          , writer: TestResponseWriter
                          }
              , components: {}
              }
      pure unit
      -- conn.response.body `shouldEqual` "OK, I've saved that for ya."
