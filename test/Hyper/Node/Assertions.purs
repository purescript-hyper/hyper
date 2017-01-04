module Hyper.Node.Assertions where

import Prelude
import Node.Buffer as Buffer
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Hyper.Node.Test (TestResponseBody(TestResponseBody))
import Hyper.Test.TestServer (TestResponse, testBody)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

assertBody
  :: forall e.
     (String -> String -> Aff (buffer :: BUFFER | e) Unit)
  -> TestResponse TestResponseBody
  -> String
  -> Aff (buffer :: BUFFER | e) Unit
assertBody assertion response expected =
  case testBody response of
    TestResponseBody chunks -> do
      body <- liftEff (Buffer.concat chunks >>= Buffer.toString UTF8)
      body `assertion` expected

bodyShouldEqual
  :: forall e.
     TestResponse TestResponseBody
  -> String
  -> Aff (buffer :: BUFFER | e) Unit
bodyShouldEqual = assertBody shouldEqual

bodyShouldContain
  :: forall e.
     TestResponse TestResponseBody
  -> String
  -> Aff (buffer :: BUFFER | e) Unit
bodyShouldContain = assertBody shouldContain
