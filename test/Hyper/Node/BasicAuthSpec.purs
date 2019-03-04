module Hyper.Node.BasicAuthSpec where

import Prelude
import Control.Monad.Indexed (ibind)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap, class Newtype)
import Data.Tuple (fst, Tuple(Tuple))
import Foreign.Object as Object
import Hyper.Middleware (evalMiddleware)
import Hyper.Middleware.Class (getConn)
import Hyper.Node.BasicAuth (authenticated, withAuthentication)
import Hyper.Response (headers, respond, writeStatus)
import Hyper.Status (statusOK)
import Hyper.Test.TestServer (TestRequest(..), TestResponse(..), defaultRequest, testHeaders, testServer, testStringBody)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)

newtype User = User String

derive instance newtypeUser :: Newtype User _

spec :: Spec Unit
spec =
  describe "Hyper.Node.BasicAuth" do

    describe "withAuthentication" do

      it "extracts basic authentication from header when correct" do
        response <- { request: TestRequest (defaultRequest { headers = Object.singleton "authorization" "Basic dXNlcjpwYXNz" })
                    , response: TestResponse Nothing [] []
                    , components: { authentication: unit }
                    }
                    # evalMiddleware (withAuthentication (pure <<< Just))
        response.components.authentication `shouldEqual` Just (Tuple "user" "pass")

      it "extracts no information if the header is missing" do
        response <- { request: TestRequest defaultRequest
                    , response: TestResponse Nothing [] []
                    , components: { authentication: unit }
                    }
                    # evalMiddleware (withAuthentication (pure <<< Just))
        response.components.authentication `shouldEqual` Nothing

      it "extracts no information if the header lacks the \"Basic\" string" do
        response <- { request: TestRequest (defaultRequest { headers = Object.singleton "authorization" "dXNlcjpwYXNz" })
                    , response: TestResponse Nothing [] []
                    , components: { authentication: unit }
                    }
                    # evalMiddleware (withAuthentication (pure <<< Just))
        response.components.authentication `shouldEqual` Nothing

      it "uses the value returned by the mapper function" do
        response <- { request: TestRequest (defaultRequest { headers = Object.singleton "authorization" "Basic dXNlcjpwYXNz" })
                    , response: TestResponse Nothing [] []
                    , components: { authentication: unit }
                    }
                    # evalMiddleware (withAuthentication (pure <<< Just <<< User <<< fst))
        map unwrap response.components.authentication `shouldEqual` Just "user"

    describe "authenticated" do
      let respondUserName = do
            conn <- getConn
            _ <- writeStatus statusOK
            _ <- headers []
            respond (unwrap conn.components.authentication)
            where bind = ibind

      it "runs the middleware with the authenticated user when available" do
        conn <- { request: TestRequest defaultRequest
                , response: TestResponse Nothing [] []
                , components: { authentication: Just (User "Alice") }
                }
                # evalMiddleware (authenticated "Test" respondUserName)
                # testServer
        testStringBody conn `shouldEqual` "Alice"

      it "sends WWW-Authenticate header when no authentication is available" do
        conn <- { request: TestRequest defaultRequest
                , response: TestResponse Nothing [] []
                , components: { authentication: Nothing }
                }
                # evalMiddleware (authenticated "Test" respondUserName)
                # testServer
        testHeaders conn `shouldEqual` [Tuple "WWW-Authenticate" "Basic realm=\"Test\""]
        testStringBody conn `shouldEqual` "Please authenticate."
