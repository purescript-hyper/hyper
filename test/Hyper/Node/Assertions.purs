module Hyper.Node.Assertions where

import Prelude
import Node.Buffer as Buffer
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Hyper.Node.Test (TestResponseBody(TestResponseBody))
import Hyper.Test.TestServer (TestResponse, testBody)
import Node.Encoding (Encoding(UTF8))
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

assertBody
  :: forall state
   . (String -> String -> Aff Unit)
  -> TestResponse TestResponseBody state
  -> String
  -> Aff Unit
assertBody assertion response expected =
  case testBody response of
    TestResponseBody chunks -> do
      body <- liftEffect (Buffer.concat chunks >>= Buffer.toString UTF8)
      body `assertion` expected

bodyShouldEqual
  :: forall state
   . TestResponse TestResponseBody state
  -> String
  -> Aff Unit
bodyShouldEqual = assertBody shouldEqual

bodyShouldContain
  :: forall state
   . TestResponse TestResponseBody state
  -> String
  -> Aff Unit
bodyShouldContain = assertBody shouldContain
