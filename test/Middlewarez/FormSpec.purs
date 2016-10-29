module Middlewarez.FormSpec where

import Prelude
import Middlewarez.Conn (HTTP)
import Middlewarez.Form (formParser, Form(Form))
import Middlewarez.Stream (fromString)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec (http :: HTTP | e) Unit
spec =
  describe "Middlewarez.Conn" do
    it "can parse the request body as a form" do
      conn <- formParser
              { request: { bodyStream: fromString "foo=bar"
                         , headers: { "content-type": "www-form-urlencoded"
                                    , "content-length": "7"
                                    }
                         }
              , response: {}
              }
      conn.request.body `shouldEqual` Form []
