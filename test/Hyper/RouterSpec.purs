module Hyper.RouterSpec where

import Prelude
import Hyper.Method (Method(..))
import Hyper.Response (notFound, respond)
import Hyper.Router (fallbackTo, handler, resource)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec e Unit
spec = do
  let greetings =
        { path: []
        , "GET": handler (respond "Hello!")
        , "POST": handler (respond "OK, I've saved that for ya.")
        }
  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      conn <- (fallbackTo notFound $ resource greetings)
              { request: { method: GET
                         , path: []
                         }
              , response: { body: unit }
              , components: {}
              }
      conn.response.body `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      conn <- (fallbackTo notFound $ resource greetings)
              { request: { method: POST
                         , path: []
                         }
              , response: { body: unit }
              , components: {}
              }
      conn.response.body `shouldEqual` "OK, I've saved that for ya."
