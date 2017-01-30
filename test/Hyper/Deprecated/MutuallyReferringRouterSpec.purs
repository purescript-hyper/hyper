module Hyper.Deprecated.MutuallyReferringRouterSpec where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Eff.Console (CONSOLE)
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Hyper.Core (StatusLineOpen, closeHeaders, writeStatus, class ResponseWriter, Conn, Middleware, ResponseEnded)
import Hyper.HTML (asString, text)
import Hyper.Method (Method(GET))
import Hyper.Response (class Response, respond, contentType)
import Hyper.Routing.ResourceRouter (ResourceRecord, Supported, Unsupported, defaultRouterFallbacks, handler, linkTo, resource, router, runRouter)
import Hyper.Status (statusOK)
import Hyper.Test.TestServer (StringBody, testStringBody, TestResponse, testResponseWriter, testHeaders, testServer)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)


-- To have `app` and its routes be polymorphic, and still compile, we need to provide
-- some type annotations. The following alias is a handy shortcut for the resources
-- used in this test suite.

type TestResource m rw get post =
  forall req res c b.
  (Monad m, ResponseWriter rw m b) =>
  ResourceRecord
  m
  Unsupported
  get
  post
  Unsupported
  Unsupported
  Unsupported
  Unsupported
  Unsupported
  (Conn { url :: String, method :: Method | req } { writer :: rw StatusLineOpen | res } c)
  (Conn { url :: String, method :: Method | req } { writer :: rw ResponseEnded | res } c)


app :: forall m req res rw b c.
  (Monad m, ResponseWriter rw m b, Response b m String) =>
  Middleware
  m
  (Conn { url :: String, method :: Method | req }
        { writer :: rw StatusLineOpen | res }
        c)
  (Conn { url :: String, method :: Method | req }
        { writer :: rw ResponseEnded | res }
        c)
app = runRouter defaultRouterFallbacks (router about <|> router contact)
  where
    about :: TestResource m rw Supported Unsupported
    about =
      resource
      { path = ["about"]
      , "GET" = handler (\conn ->
                         writeStatus statusOK conn
                         >>= contentType textHTML
                         >>= closeHeaders
                         >>= respond (asString (linkTo (contact ∷ TestResource m rw Supported Unsupported) [text "Contact Me!"])))
      }

    contact :: TestResource m rw Supported Unsupported
    contact =
      resource
      { path = ["contact"]
      , "GET" = handler (\conn ->
                          writeStatus statusOK conn
                          >>= contentType textHTML
                          >>= closeHeaders
                          >>= respond (asString (linkTo (about ∷ TestResource m rw Supported Unsupported) [text "About Me"])))
      }


getResponse
  :: forall m.
     (Monad m, Response StringBody m String) =>
     Method ->
     String ->
     m (TestResponse StringBody)
getResponse method url =
  let conn = { request: { method: method
                        , url: url
                        }
             , response: { writer: testResponseWriter }
             , components: {}
             }
  in testServer (app conn)


spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  describe "Hyper.HTML.DSL" do
    it "can linkTo an existing route" do
      response <- getResponse GET "about"
      testHeaders response `shouldEqual` [Tuple "Content-Type" "text/html"]
      testStringBody response `shouldEqual` "<a href=\"/contact\">Contact Me!</a>"

    it "can linkTo another existing route" do
      response <- getResponse GET "contact"
      testHeaders response `shouldEqual` [Tuple "Content-Type" "text/html"]
      testStringBody response `shouldEqual` "<a href=\"/about\">About Me</a>"
