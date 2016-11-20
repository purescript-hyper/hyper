module Hyper.HTML.DSLSpec where

import Prelude
import Hyper.Conn ((??>), fallbackTo, HTTP)
import Hyper.HTML.DSL (text, linkTo, html)
import Hyper.Method (Method(POST, GET))
import Hyper.Response (notFound)
import Hyper.Router (Unsupported(Unsupported), MethodHandler(NotRouted, Routed), Supported(Supported), ResourceMethod(ResourceMethod), resource)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

about =
  { path: ["about"]
  , "GET": ResourceMethod Supported (Routed (\conn -> html (linkTo contact (text "Contact Me!")) conn))
  , "POST": ResourceMethod Unsupported NotRouted
  }

contact =
  { path: ["contact"]
  , "GET": ResourceMethod Supported (Routed (\conn -> html (linkTo about (text "About Me")) conn))
  , "POST": ResourceMethod Unsupported NotRouted
  }

spec :: forall e. Spec (http :: HTTP | e) Unit
spec = do
  let app = fallbackTo notFound $ (resource about ??> resource contact)

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
