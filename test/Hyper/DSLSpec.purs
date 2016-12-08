module Hyper.HTML.DSLSpec where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Hyper.Core (HeadersClosed(HeadersClosed), class ResponseWriter, Middleware, HeadersOpen(HeadersOpen), ResponseEnded(ResponseEnded), Conn)
import Hyper.HTML.DSL (text, linkTo, html)
import Hyper.Method (Method(GET))
import Hyper.Response (headers, notFound)
import Hyper.Router (Path, notSupported, Unsupported, Supported, ResourceRecord, fallbackTo, handler, resource)
import Hyper.TestUtilities (TestResponseWriter(TestResponseWriter))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

about :: forall m req res rw c.
         (Monad m, ResponseWriter rw m) =>
         ResourceRecord
         m
         Supported
         Unsupported
         (Conn { path :: Path, method :: Method | req } { writer :: rw, state :: HeadersClosed | res } c)
         (Conn { path :: Path, method :: Method | req } { writer :: rw, state :: ResponseEnded | res } c)
about = -- THIS IS WHERE I GET AN ERROR
   { path: ["about"]
  , "GET": handler (\conn -> html (linkTo contact (text "Contact Me!")) conn) -- SEEMINGLY CAUSED BY THIS
  , "POST": notSupported
  }

contact :: forall m req res rw c.
           (Monad m, ResponseWriter rw m) =>
           ResourceRecord
           m
           Supported
           Unsupported
           (Conn { path :: Path, method :: Method | req } { writer :: rw, state :: HeadersClosed | res } c)
           (Conn { path :: Path, method :: Method | req } { writer :: rw, state :: ResponseEnded | res } c)  
contact =
  { path: ["contact"]
  , "GET": handler (html (text "No"))
  --, "GET": handler (\conn -> html (linkTo about (text "About Me")) conn)
  , "POST": notSupported
  }

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
app = headers [] >=> (fallbackTo (notFound) (resource about <|> resource contact))

-- But the rest is nice!

spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  describe "Hyper.HTML.DSL" do
    it "can linkTo an existing route" do
      conn <- app
              { request: { method: GET
                         , path: ["about"]
                         }
              , response: { state: HeadersOpen
                          , writer: TestResponseWriter
                          }
              , components: {}
              }
      -- conn.response.body `shouldEqual` "<a href=\"/contact\">Contact Me!</a>"
      pure unit

    it "can linkTo another existing route" do
      conn <- app
              { request: { method: GET
                         , path: ["contact"]
                         }
              , response: { state: HeadersOpen
                          , writer: TestResponseWriter
                          }
              , components: {}
              }
      -- conn.response.body `shouldEqual` "<a href=\"/about\">About Me</a>"
      pure unit
