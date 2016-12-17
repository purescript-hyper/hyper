module Hyper.FormSpec where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.StrMap (singleton)
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
      form <- parseForm
              { request: { body: "foo=bar"
                         , headers: singleton "content-type" "application/x-www-form-urlencoded; charset=utf8"
                         }
              , response: {}
              , components: {}
              }
              # map _.request.body
              >>= liftEither
      form `shouldEqual` (Form [Tuple "foo" "bar"])

    it "fails to parse request body as a form when invalid" $ expectError do
      parseForm { request: { body: "foo=bar=baz"
                           , headers: singleton "content-type" "application/x-www-form-urlencoded; charset=utf8"
                           }
                , response: {}
                , components: {}
                }
        # map _.request.body
        >>= liftEither
