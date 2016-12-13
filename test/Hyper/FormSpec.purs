module Hyper.FormSpec where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Tuple (Tuple(Tuple))
import Hyper.BodyParser (parse)
import Hyper.Form (FormParser(FormParser), Form(Form))
import Node.Buffer (BUFFER, fromString)
import Node.Encoding (Encoding(..))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

spec :: forall e. Spec (buffer ∷ BUFFER | e) Unit
spec =
  describe "Hyper.Form" do
    it "can parse the request body as a form" do
      body ← liftEff (fromString "foo=bar" UTF8)
      conn <- (parse FormParser)
              { request: { body: body
                           -- Headers required by FormParser are 'content-type' and 'content-length'
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
      (parse FormParser)
      { request: { body: fromString "foo=bar=baz"
                   -- Headers required by formParser (content-type, content-length):
                 , headers: { "content-type": "application/x-www-form-urlencoded; charset=utf8"
                            , "content-length": "11"
                            }
                 }
      , response: {}
      , components: {}
      }
