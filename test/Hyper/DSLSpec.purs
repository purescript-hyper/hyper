module Hyper.HTML.DSLSpec where

import Prelude
import Control.Alt ((<|>))
import Hyper.HTML.DSL (text, linkTo, html)
import Hyper.Method (Method(GET))
import Hyper.Response (notFound)
import Hyper.Router (fallbackTo, notSupported, handler, resource)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

about =
  { path: ["about"]
  , "GET": handler (\conn -> (html (linkTo contact (text "Contact Me!"))) conn)
  , "POST": notSupported
  }

contact =
  { path: ["contact"]
  , "GET": handler (\conn -> (html (linkTo about (text "About Me"))) conn)
  , "POST": notSupported
  }

app = fallbackTo notFound (resource about <|> resource contact)

spec :: forall e. Spec e Unit
spec = do
  describe "Hyper.HTML.DSL" do
    it "can linkTo an existing route" do
      conn <- app
              { request: { method: GET
                         , path: ["about"]
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "<a href=\"/contact\">Contact Me!</a>"

    it "can linkTo another existing route" do
      conn <- app
              { request: { method: GET
                         , path: ["contact"]
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "<a href=\"/about\">About Me</a>"
