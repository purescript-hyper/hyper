module Hyper.HTML.DSLSpec where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Eff.Console (CONSOLE)
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Hyper.Core (statusOK, StatusLineOpen, closeHeaders, statusNotFound, writeStatus, class ResponseWriter, Conn, Middleware, ResponseEnded)
import Hyper.HTML (text)
import Hyper.Method (Method(GET))
import Hyper.Response (respond, contentType)
import Hyper.Router (linkTo, notSupported, Unsupported, Supported, ResourceRecord, fallbackTo, handler, resource)
import Hyper.Test.TestServer (testResponseWriter, testBody, testHeaders, testServer)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

-- To have `app` and its routes be polymorphic, and still compile, we need to provide
-- some type annotations. The following alias is a handy shortcut for the resources
-- used in this test suite.

type TestResource m rw gr pr =
  forall req res c.
  (Monad m, ResponseWriter rw m) =>
  ResourceRecord
  m
  gr
  pr
  (Conn { url :: String, method :: Method | req } { writer :: rw StatusLineOpen | res } c)
  (Conn { url :: String, method :: Method | req } { writer :: rw ResponseEnded | res } c)

app :: forall m req res rw c.
  (Monad m, ResponseWriter rw m) =>
  Middleware
  m
  (Conn { url :: String, method :: Method | req }
        { writer :: rw StatusLineOpen | res }
        c)
  (Conn { url :: String, method :: Method | req }
        { writer :: rw ResponseEnded | res }
        c)
app = fallbackTo notFound (resource about <|> resource contact)
  where
    notFound =
      writeStatus statusNotFound
      >=> contentType textHTML
      >=> closeHeaders
      >=> respond (text "Not Found")
    about :: TestResource m rw Supported Unsupported
    about =
      { path: ["about"]
      , "GET": handler (\conn ->
                         writeStatus statusOK conn
                         >>= contentType textHTML
                         >>= closeHeaders
                         >>= respond (linkTo (contact ∷ TestResource m rw Supported Unsupported) [text "Contact Me!"]))
      , "POST": notSupported
      }

    contact :: TestResource m rw Supported Unsupported
    contact =
      { path: ["contact"]
      , "GET": handler (\conn ->
                         writeStatus statusOK conn
                         >>= contentType textHTML
                         >>= closeHeaders
                         >>= respond (linkTo (about ∷ TestResource m rw Supported Unsupported) [text "About Me"]))
      , "POST": notSupported
      }

spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  describe "Hyper.HTML.DSL" do
    it "can linkTo an existing route" do
      response <- { request: { method: GET
                             , url: "about"
                             }
                  , response: { writer: testResponseWriter }
                  , components: {}
                  }
                  # app
                  # testServer
      testHeaders response `shouldEqual` [Tuple "Content-Type" "text/html"]
      testBody response `shouldEqual` "<a href=\"/contact\">Contact Me!</a>"

    it "can linkTo another existing route" do
      response <- { request: { method: GET
                             , url: "contact"
                             }
                  , response: { writer: testResponseWriter }
                  , components: {}
                  }
                  # app
                  # testServer
      testHeaders response `shouldEqual` [Tuple "Content-Type" "text/html"]
      testBody response `shouldEqual` "<a href=\"/about\">About Me</a>"
