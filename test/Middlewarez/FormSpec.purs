module Middlewarez.FormSpec where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Middlewarez.Conn (HTTP)
import Middlewarez.Form (formParser, Form(Form))
import Middlewarez.Stream (fromString)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec (http :: HTTP | e) Unit
spec =
  describe "Middlewarez.Form" do
    it "can parse the request body as a form" do
      conn <- formParser
              { request: { bodyStream: fromString "foo=bar"
                           -- Headers required by formParser (content-type, content-length):
                         , headers: { "content-type": "application/x-www-form-urlencoded; charset=utf8"
                                    , "content-length": "7"
                                    -- Other headers are OK too.
                                    , "host": "localhost"
                                    , "user-agent": "test"
                                    }
                         }
              , response: {}
              }
      conn.request.body `shouldEqual` Form [Tuple "foo" "bar"]
