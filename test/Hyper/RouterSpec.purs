module Hyper.RouterSpec where

import Prelude
import Hyper.Conn (ResponseMiddleware, HTTP)
import Hyper.Method (Method(POST, GET))
import Hyper.Router (router)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

respond :: forall e res b c. String
           -> ResponseMiddleware e { body :: b | res } { body :: String | res } c
respond s conn@{ response: r } = 
  pure (conn { response = ( r { body = s }) })

spec :: forall e. Spec (http :: HTTP | e) Unit
spec =
  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      conn <- (router { "GET": respond "Hello!" })
              { request: { method: GET
                         , path: ""
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      conn <- (router { "POST": respond "OK, I've saved that for ya." })
              { request: { method: POST
                         , path: ""
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "OK, I've saved that for ya."
