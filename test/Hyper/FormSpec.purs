module Hyper.FormSpec where

import Prelude
import Effect.Aff (Aff)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(Tuple), fst)
import Foreign.Object (singleton)
import Hyper.Form (Form(Form), parseForm)
import Hyper.Middleware (runMiddleware)
import Hyper.Test.TestServer (TestRequest(TestRequest))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (expectError, shouldEqual)

liftEither ∷ ∀ a. Either String a → Aff a
liftEither e =
  case e of
    Left err → throwError (error err)
    Right x → pure x

spec :: Spec Unit
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
