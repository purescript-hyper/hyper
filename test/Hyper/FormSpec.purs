module Hyper.FormSpec where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (singleton)
import Data.Tuple (Tuple(Tuple), fst)
import Hyper.Form (Form(Form), parseForm)
import Hyper.Middleware (runMiddleware)
import Hyper.Test.TestServer (TestRequest(TestRequest))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

liftEither ∷ ∀ e a. Either String a → Aff e a
liftEither e =
  case e of
    Left err → throwError (error err)
    Right x → pure x

spec :: forall e. Spec e Unit
spec =
  describe "Hyper.Form" do
    it "parses key without value" do
      form <- runParseForm "foo" Nothing
      form `shouldEqual` (Form [Tuple "foo" Nothing])

    it "parses multiple keys without values" do
      form <- runParseForm "foo&foo&bar&foo" Nothing
      form `shouldEqual` (Form [ Tuple "foo" Nothing
                               , Tuple "foo" Nothing
                               , Tuple "bar" Nothing
                               , Tuple "foo" Nothing
                               ])

    it "parses key and value" do
      form <- runParseForm "foo=bar" Nothing
      form `shouldEqual` (Form [Tuple "foo" (Just "bar")])

    it "handles percent-encoding" do
      form <- runParseForm "foo=%62%61%72" Nothing
      form `shouldEqual` (Form [Tuple "foo" (Just "bar")])

    it "parses multiple keys and values" do
      form <- runParseForm "foo=bar&baz=quux&a=1&b=2" Nothing
      form `shouldEqual` (Form [ Tuple "foo" (Just "bar")
                               , Tuple "baz" (Just "quux")
                               , Tuple "a" (Just "1")
                               , Tuple "b" (Just "2")
                               ])

    it "fails to parse request body as a form when invalid" $ expectError $
      runParseForm "foo=bar=baz" Nothing

  where
    runParseForm body contentType =
      runMiddleware
      parseForm
      { request: TestRequest { method: Left GET
                             , body: body
                             , url: ""
                             , headers: singleton
                                         "content-type"
                                         (fromMaybe
                                         "application/x-www-form-urlencoded; charset=utf8"
                                         contentType)
                             }
      , response: {}
      , components: {}
      }
      # map fst
      >>= liftEither
