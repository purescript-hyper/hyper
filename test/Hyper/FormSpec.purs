module Hyper.FormSpec where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Hyper.BodyParser (parse)
import Hyper.Conn (HTTP)
import Hyper.Form (FormParser(FormParser), Form(Form))
import Hyper.Stream (fromString)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

spec :: forall e. Spec (http :: HTTP | e) Unit
spec =
  describe "Hyper.Form" do
    it "can parse the request body as a form" do
      conn <- parse
              FormParser
              { request: { body: fromString "foo=bar"
                           -- Headers required by formParser (content-type, content-length):
                         , headers: { "content-type": "application/x-www-form-urlencoded; charset=utf8"
                                    , "content-length": "7"
                                    -- Other headers are OK too.
                                    , "host": "localhost"
                                    , "user-agent": "test"
                                    }
                         }
              , response: {}
              , components: {}
              }
      conn.request.body `shouldEqual` Form [Tuple "foo" "bar"]
    it "fails to parse request body as a form when invalid" $ expectError $
      parse
      FormParser
      { request: { body: fromString "foo=bar=baz"
                   -- Headers required by formParser (content-type, content-length):
                 , headers: { "content-type": "application/x-www-form-urlencoded; charset=utf8"
                            , "content-length": "11"
                            }
                 }
        , response: {}
        , components: {}
        }
