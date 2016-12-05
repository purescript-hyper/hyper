module Hyper.HTML.DSLSpec where

import Prelude
import Control.Alt ((<|>))
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Hyper.Conn (Conn)
import Hyper.HTML.DSL (text, linkTo, html)
import Hyper.Method (Method(GET))
import Hyper.Response (notFound)
import Hyper.Router (notSupported, Unsupported, Supported, ResourceRecord, fallbackTo, handler, resource)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

-- Still quite verbose, but does not type check without this:

type GetResource =
  forall req res c.
  ResourceRecord
  Identity
  Supported
  Unsupported
  (Conn req { | res } c)
  (Conn req { body :: String | res } c)

-- But the rest is nice!

spec :: forall e. Spec e Unit
spec = do
  let
    about :: GetResource
    about =
      { path: ["about"]
      , "GET": handler (\conn -> html (linkTo contact (text "Contact Me!")) conn)
      , "POST": notSupported
      }
  
    contact :: GetResource
    contact =
      { path: ["contact"]
      , "GET": handler (\conn -> (html (linkTo about (text "About Me"))) conn)
      , "POST": notSupported
      }

    app = fallbackTo notFound (resource about <|> resource contact)
 
  describe "Hyper.HTML.DSL" do
    it "can linkTo an existing route" do
      let conn =
              { request: { method: GET
                         , path: ["about"]
                         }
              , response: {}
              , components: {}
              }
              # app
              # unwrap
      conn.response.body `shouldEqual` "<a href=\"/contact\">Contact Me!</a>"

    it "can linkTo another existing route" do
      let conn =
              { request: { method: GET
                         , path: ["contact"]
                         }
              , response: {}
              , components: {}
              }
              # app
              # unwrap
      conn.response.body `shouldEqual` "<a href=\"/about\">About Me</a>"
