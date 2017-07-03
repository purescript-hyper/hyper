module Hyper.CookiesSpec where

import Prelude

import Control.Alternative (empty)
import Data.Array ((:))
import Data.Either (Either(..), either, isLeft)
import Data.JSDate (jsdate)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (fromNonEmpty, (:|))
import Data.Set as Set
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Hyper.Cookies (SameSite(..), cookies, defaultCookieAttributes, maxAge, setCookie)
import Hyper.Middleware (evalMiddleware)
import Hyper.Test.TestServer (TestRequest(..), TestResponse(..), defaultRequest, testHeaders, testServer)
import Node.Buffer (BUFFER)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec (buffer :: BUFFER | e) Unit
spec = do

  describe "Hyper.Node.Cookies" do

    describe "cookies" do

      it "parses a no cookies" do
        response <- parseCookies ""
        response.components.cookies
          `shouldEqual`
          Right StrMap.empty

      it "parses a single cookie" do
        response <- parseCookies "foo=1"
        response.components.cookies
          `shouldEqual`
          Right (StrMap.singleton "foo" ("1" :| empty))

      it "parses multiple cookies" do
        response <- parseCookies "foo=1;bar=2;baz=3"
        response.components.cookies
          `shouldEqual`
          Right (StrMap.fromFoldable
                 [ Tuple "foo" ("1" :| empty)
                 , Tuple "bar" ("2" :| empty)
                 , Tuple "baz" ("3" :| empty)
                 ])

      it "parses multiple cookie values for same key" do
        values <- parseCookies "foo=1;bar=2;foo=3"
                  # map (cookieValues "foo")
        values `shouldEqual` Just (Set.fromFoldable ["1", "3"])

      it "ignores blanks" do
        conn <- parseCookies "     ;  ;foo=3; ; ; ;;;"
        conn.components.cookies
          `shouldEqual`
          Right (StrMap.singleton "foo" ("3" :| empty))

      it "fails on invalid pairs" do
        conn <- parseCookies "foo"
        isLeft conn.components.cookies `shouldEqual` true

      it "fails on triples" do
        conn <- parseCookies "foo=bar=baz"
        isLeft conn.components.cookies `shouldEqual` true

    describe "setCookie" do

      it "sets a simple cookie" do
        response <- { request: TestRequest defaultRequest
                    , response: TestResponse Nothing [] []
                    , components: {}
                    }
                    # evalMiddleware (setCookie "foo" "bar" defaultCookieAttributes)
                    # testServer
        testHeaders response `shouldEqual` [Tuple "Set-Cookie" "foo=bar"]

      it "sets cookie with attributes" do
        let
          expires =
            jsdate
              { year : 2017.0
              , month : 7.0
              , day : 4.0
              , hour : 0.0
              , minute : 40.0
              , second : 0.0
              , millisecond : 0.0
              }
          attrs =
            { expires: Just expires
            , maxAge : maxAge 3600
            , domain : Just "localhost"
            , path : Just "/path"
            , secure : true
            , httpOnly : true
            , sameSite : Just Strict
            }
        response <- { request: TestRequest defaultRequest
                    , response: TestResponse Nothing [] []
                    , components: {}
                    }
                    # evalMiddleware (setCookie "foo" "bar" attrs)
                    # testServer
        (shouldEqual
          (testHeaders response)
          [(Tuple
            "Set-Cookie"
            "foo=bar;HttpOnly;Secure;Expires=Fri, 04 Aug 2017 00:40:00 GMT;Max-Age=3600;Domain=localhost;Path=/path;SameSite=Strict")])

      it "URL encodes cookie key" do
        response <- { request: TestRequest defaultRequest
                    , response: TestResponse Nothing [] []
                    , components: {}
                    }
                    # evalMiddleware (setCookie "&stuff!we like" "bar" defaultCookieAttributes)
                    # testServer
        testHeaders response `shouldEqual` [Tuple "Set-Cookie" "%26stuff!we%20like=bar"]

      it "URL encodes cookie value" do
        response <- { request: TestRequest defaultRequest
                    , response: TestResponse Nothing [] []
                    , components: {}
                    }
                    # evalMiddleware (setCookie "yeah" "=& ?%" defaultCookieAttributes)
                    # testServer
        testHeaders response `shouldEqual` [Tuple "Set-Cookie" "yeah=%3D%26%20%3F%25"]

  where
    parseCookies s =
      { request: TestRequest
                 (defaultRequest { headers = StrMap.singleton "cookie" s })
      , response: {}
      , components: { cookies: unit }
      }
      # evalMiddleware cookies

    cookieValues key =
      _.components.cookies
      >>> either (const StrMap.empty) id
      >>> StrMap.lookup key
      >>> map (fromNonEmpty (:))
      >>> map Set.fromFoldable

