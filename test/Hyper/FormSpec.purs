module Hyper.FormSpec where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit)
import Hyper.BodyParser (parse)
import Hyper.Form (Form(Form), parseForm)
import Node.Buffer (BUFFER, fromString)
import Node.Encoding (Encoding(..))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

failOnLeft ∷ ∀ e r. Either Error r → Aff e Unit
failOnLeft e =
  case e of
    Left err → throwError err
    Right x → pure unit
    
spec :: forall e. Spec e Unit
spec =
  describe "Hyper.Form" do
    it "can parse the request body as a form" do
      conn <- parseForm
              { request: { body: "foo=bar"
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
      conn.request.body `shouldEqual` Right (Form [Tuple "foo" "bar"])
      
    it "fails to parse request body as a form when invalid" $ expectError do
      conn ← parseForm
             { request: { body: "foo=bar=baz"
                          -- Headers required by formParser (content-type, content-length):
                        , headers: { "content-type": "application/x-www-form-urlencoded; charset=utf8"
                                   , "content-length": "11"
                                   }
                        }
             , response: {}
             , components: {}
             }
      failOnLeft conn.request.body


