module Hyper.Node.BasicAuthSpec where

import Prelude
import Data.StrMap as StrMap
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap, class Newtype)
import Data.Tuple (fst, Tuple(Tuple))
import Hyper.Core (statusOK, writeStatus)
import Hyper.Node.BasicAuth (authenticated, withAuthentication)
import Hyper.Response (headers, respond)
import Hyper.Test.TestServer (testHeaders, testBody, testServer, testResponseWriter)
import Node.Buffer (BUFFER)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)

newtype User = User String

derive instance newtypeUser :: Newtype User _

spec :: forall e. Spec (buffer :: BUFFER | e) Unit
spec =
  describe "Hyper.Node.BasicAuth" do

    describe "withAuthentication" do

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
                    # withAuthentication (pure <<< Just <<< User <<< fst)
        map unwrap response.components.authentication `shouldEqual` Just "user"

    describe "authenticated" do
      let respondUserName conn =
            writeStatus statusOK conn
            >>= headers []
            >>= respond (unwrap conn.components.authentication)

      it "runs the middleware with the authenticated user when available" do
        conn <- { request: {}
                , response: { writer: testResponseWriter }
                , components: { authentication: Just (User "Alice") }
                }
                # authenticated "Test" respondUserName
                # testServer
        testBody conn `shouldEqual` "Alice"

      it "sends WWW-Authenticate header when no authentication is available" do
        conn <- { request: {}
                , response: { writer: testResponseWriter }
                , components: { authentication: Nothing }
                }
                # authenticated "Test" respondUserName
                # testServer
        testHeaders conn `shouldEqual` [Tuple "WWW-Authenticate" "Basic realm=\"Test\""]
        testBody conn `shouldEqual` "Please authenticate."
