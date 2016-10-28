module Middlewarez.ConnSpec where

import Prelude
import Middlewarez.Conn (Form(Form), formParser)
import Middlewarez.Stream (stdin)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "Middlewarez.Conn" do
    it "can parse the request body as a form" do
      let conn = formParser ({ bodyStream: stdin })
      conn.body `shouldEqual` Form
