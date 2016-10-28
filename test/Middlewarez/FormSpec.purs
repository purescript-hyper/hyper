module Middlewarez.FormSpec where

import Prelude
import Middlewarez.Stream (someStream)
import Middlewarez.Form (formParser, Form(Form))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "Middlewarez.Conn" do
    it "can parse the request body as a form" do
      let conn = formParser
                 { request: { bodyStream: someStream }
                 , response: {}
                 }
      conn.request.body `shouldEqual` Form
