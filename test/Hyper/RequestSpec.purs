module Hyper.RequestSpec where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Hyper.Request (parseUrl)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Hyper.Request" do
    it "parses the root URL" do
      let result = parseUrl "/"
      result.path `shouldEqual` []
      result.query `shouldEqual` Right []

    it "parses non-root URLs" do
      let result = parseUrl "/foo/bar"
      result.path `shouldEqual` ["foo", "bar"]
      result.query `shouldEqual` Right []

    it "parses URLs with query strings" do
      let result = parseUrl "/foo/bar?abc=def=ghi"
      result.path `shouldEqual` ["foo", "bar"]
      result.query `shouldEqual` Left "abc=def=ghi"

    it "parses URLs with formatted query strings" do
      let result = parseUrl "/foo/bar?abc=def&ghi"
      result.path `shouldEqual` ["foo", "bar"]
      result.query `shouldEqual` Right ["abc" /\ Just "def", "ghi" /\ Nothing]
