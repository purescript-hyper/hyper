module Hyper.Routing.ResourceRouterSpec where

import Prelude
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (StatusLineOpen, statusCreated, statusOK, closeHeaders, statusNotFound, writeStatus, class ResponseWriter, Conn, Middleware, ResponseEnded)
import Hyper.Method (Method(..))
import Hyper.Response (contentType, headers, respond)
import Hyper.Routing.ResourceRouter (router, fallbackTo, handler, resource)
import Hyper.Test.TestServer (testResponseWriter, testStatus, testBody, testServer)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

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
app = fallbackTo notFound (router greetings)
  where
    notFound =
      writeStatus statusNotFound
      >=> contentType textHTML
      >=> closeHeaders
      >=> respond "Not Found"
    greetings =
      resource
      { path = []
      , "GET" = handler (writeStatus statusOK
                        >=> headers []
                        >=> respond "Hello!")
      , "POST" = handler (writeStatus statusCreated
                         >=> headers []
                         >=> respond "OK, I've saved that for ya.")
      }

spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      response ←
        { request: { method: GET
                   , url: ""
                   }
        , response: { writer: testResponseWriter }
        , components: {}
        }
        # app
        # testServer
      testBody response `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      response ←
            { request: { method: POST
                       , url: ""
                       }
            , response: { writer: testResponseWriter }
            , components: {}
            }
            # app
            # testServer
      testStatus response `shouldEqual` Just (Tuple 201 "Created")
      testBody response `shouldEqual` "OK, I've saved that for ya."
