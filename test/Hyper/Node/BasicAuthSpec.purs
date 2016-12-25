module Hyper.Node.BasicAuthSpec where

import Prelude
import Data.StrMap as StrMap
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (fst, Tuple(Tuple))
import Hyper.Node.BasicAuth (withAuthentication)
import Hyper.Test.TestServer (testResponseWriter)
import Node.Buffer (BUFFER)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec (buffer :: BUFFER | e) Unit
spec =
  describe "Hyper.Node.BasicAuth" do

    it "extracts basic authentication from header when correct" do
      response <- { request: { headers: StrMap.singleton "authorization" "Basic dXNlcjpwYXNz" }
                  , response: { writer: testResponseWriter }
                  , components: { authentication: unit }
                  }
                  # withAuthentication (pure <<< Just)
      response.components.authentication `shouldEqual` Just (Tuple "user" "pass")

    it "extracts no information if the header is missing" do
      response <- { request: { headers: StrMap.empty }
                  , response: { writer: testResponseWriter }
                  , components: { authentication: unit }
                  }
                  # withAuthentication (pure <<< Just)
      response.components.authentication `shouldEqual` Nothing

    it "extracts no information if the header lacks the \"Basic\" string" do
      response <- { request: { headers: StrMap.singleton "authorization" "dXNlcjpwYXNz" }
                  , response: { writer: testResponseWriter }
                  , components: { authentication: unit }
                  }
                  # withAuthentication (pure <<< Just)
      response.components.authentication `shouldEqual` Nothing

    it "uses the value returned by the mapper function" do
      response <- { request: { headers: StrMap.singleton "authorization" "Basic dXNlcjpwYXNz" }
                  , response: { writer: testResponseWriter }
                  , components: { authentication: unit }
                  }
                  # withAuthentication (pure <<< Just <<< fst)
      response.components.authentication `shouldEqual` (Just "user")
