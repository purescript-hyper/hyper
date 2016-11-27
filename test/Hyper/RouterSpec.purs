module Hyper.RouterSpec where

import Prelude
import Hyper.Conn (HTTP)
import Hyper.Method (Method(..))
import Hyper.Middleware (runMiddlewareT)
import Hyper.Response (notFound, respond)
import Hyper.Router (fallbackTo, handler, resource)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

greetings =
  { path: []
  , "GET": handler (respond "Hello!")
  , "POST": handler (respond "OK, I've saved that for ya.")
  }

spec :: forall e. Spec (http :: HTTP | e) Unit
spec = do
  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      conn <- runMiddlewareT
              (fallbackTo notFound $ resource greetings)
              { request: { method: GET
                         , path: []
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      conn <- runMiddlewareT
              (fallbackTo notFound $ resource greetings)
              { request: { method: POST
                         , path: []
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "OK, I've saved that for ya."
