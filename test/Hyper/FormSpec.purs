module Hyper.FormSpec where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Tuple (Tuple(Tuple))
import Hyper.Form (Form(Form), parseForm)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

liftEither ∷ ∀ e a. Either Error a → Aff e a
liftEither e =
  case e of
    Left err → throwError err
    Right x → pure x

spec :: forall e. Spec e Unit
spec =
  describe "Hyper.Form" do
    it "can parse the request body as a form" do
      conn <- parseForm
              { request: { body: "foo=bar"
                           -- Headers required by FormParser are 'content-type'
                           -- and 'content-length'.
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
              >>= liftEither
      conn.request.body `shouldEqual` (Form [Tuple "foo" "bar"])

    it "fails to parse request body as a form when invalid" $ expectError do
      parseForm { request: { body: "foo=bar=baz"
                             -- Headers required by formParser (content-type,
                             -- content-length):
                           , headers: { "content-type": "application/x-www-form-urlencoded; charset=utf8"
                                      , "content-length": "11"
                                      }
                           }
                , response: {}
                , components: {}
                }
        >>= liftEither
