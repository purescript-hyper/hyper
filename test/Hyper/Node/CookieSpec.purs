module Hyper.Node.CookieSpec where

import Prelude
import Data.Set as Set
import Data.StrMap as StrMap
import Control.Alternative (empty)
import Data.Array ((:))
import Data.Either (Either(..), either, isLeft)
import Data.Maybe (Maybe(Just))
import Data.NonEmpty (fromNonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Hyper.Middleware (evalMiddleware)
import Hyper.Node.Cookie (cookies)
import Node.Buffer (BUFFER)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec (buffer :: BUFFER | e) Unit
spec = do
  let parseCookies s =
        { request: { headers: StrMap.singleton "cookie" s }
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

  describe "Hyper.Node.Cookie" do
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
