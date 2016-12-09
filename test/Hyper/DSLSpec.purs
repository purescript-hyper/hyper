module Hyper.HTML.DSLSpec where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Eff.Console (CONSOLE)
import Data.Tuple (Tuple(..))
import Hyper.Core (class ResponseWriter, Conn, HeadersClosed, HeadersOpen(..), Middleware, ResponseEnded)
import Hyper.HTML.DSL (text, linkTo, html)
import Hyper.Method (Method(GET))
import Hyper.Response (headers, notFound)
import Hyper.Router (Path, notSupported, Unsupported, Supported, ResourceRecord, fallbackTo, handler, resource)
import Hyper.Test.TestServer (TestResponseWriter(..), testBody, testHeaders, testServer)
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
  (Conn { path :: Path, method :: Method | req } { writer :: rw, state :: HeadersClosed | res } c)
  (Conn { path :: Path, method :: Method | req } { writer :: rw, state :: ResponseEnded | res } c)

app :: forall m req res rw c.
  (Monad m, ResponseWriter rw m) =>
  Middleware
  m
  (Conn { path :: Path, method :: Method | req }
        { writer :: rw, state :: HeadersOpen | res }
        c)
  (Conn { path :: Path, method :: Method | req }
        { writer :: rw, state :: ResponseEnded | res }
        c)
app = headers [Tuple "content-type" "text/html"] 
      >=> fallbackTo (notFound) (resource about <|> resource contact)
  where
    about :: TestResource m rw Supported Unsupported
    about =
      { path: ["about"]
      , "GET": handler (\conn -> html (linkTo (contact ∷ TestResource m rw Supported Unsupported) (text "Contact Me!")) conn)
      , "POST": notSupported
      }

    contact :: TestResource m rw Supported Unsupported
    contact =
      { path: ["contact"]
      , "GET": handler (\conn -> html (linkTo (about ∷ TestResource m rw Supported Unsupported) (text "About Me")) conn)
      , "POST": notSupported
      }

spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  describe "Hyper.HTML.DSL" do
    it "can linkTo an existing route" do
      response <- { request: { method: GET
                             , path: ["about"]
                             }
                  , response: { state: HeadersOpen
                              , writer: TestResponseWriter
                              }
                  , components: {}
                  }
                  # app
                  # testServer
      testHeaders response `shouldEqual` [Tuple "content-type" "text/html"]
      testBody response `shouldEqual` "<a href=\"/contact\">Contact Me!</a>"

    it "can linkTo another existing route" do
      response <- { request: { method: GET
                             , path: ["contact"]
                             }
                  , response: { state: HeadersOpen
                              , writer: TestResponseWriter
                              }
                  , components: {}
                  }
                  # app
                  # testServer
      testHeaders response `shouldEqual` [Tuple "content-type" "text/html"]
      testBody response `shouldEqual` "<a href=\"/about\">About Me</a>"
