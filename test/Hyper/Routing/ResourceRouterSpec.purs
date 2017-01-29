module Hyper.Routing.ResourceRouterSpec where

import Prelude
import Control.Alternative ((<|>))
import Data.Maybe (Maybe(Just))
import Hyper.Core (StatusLineOpen, writeStatus, class ResponseWriter, Conn, Middleware, ResponseEnded)
import Hyper.Method (Method(..))
import Hyper.Response (class Response, headers, respond)
import Hyper.Routing.ResourceRouter (defaultRouterFallbacks, runRouter, router, handler, resource)
import Hyper.Status (statusCreated, statusMethodNotAllowed, statusOK)
import Hyper.Test.TestServer (StringBody, testStringBody, testResponseWriter, testStatus, testServer)
import Node.Buffer (BUFFER)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

app :: forall m req res rw c.
  (Monad m, Response StringBody m String, ResponseWriter rw m StringBody) =>
  Middleware
  m
  (Conn { url :: String, method :: Method | req }
        { writer :: rw StatusLineOpen | res }
        c)
  (Conn { url :: String, method :: Method | req }
        { writer :: rw ResponseEnded | res }
        c)
app = runRouter defaultRouterFallbacks (router index <|> router greetings)
  where
    index =
      resource
      { path = []
      , "GET" = handler (writeStatus statusOK
                         >=> headers []
                         >=> respond "Welcome!")
      }
    greetings =
      resource
      { path = ["greetings"]
      , "GET" = handler (writeStatus statusOK
                        >=> headers []
                        >=> respond "Hello!")
      , "POST" = handler (writeStatus statusCreated
                         >=> headers []
                         >=> respond "OK, I've saved that for ya.")
      }


spec :: forall e. Spec (buffer :: BUFFER | e) Unit
spec = do
  describe "Hyper.Routing.ResourceRouter" do
    it "can route a GET for the root resource" do
      conn ←
        { request: { method: GET
                   , url: "/greetings"
                   }
        , response: { writer: testResponseWriter }
        , components: {}
        }
        # app
        # testServer
      testStringBody conn `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      conn ←
            { request: { method: POST
                       , url: "/greetings"
                       }
            , response: { writer: testResponseWriter }
            , components: {}
            }
            # app
            # testServer
      testStatus conn `shouldEqual` Just statusCreated
      testStringBody conn `shouldEqual` "OK, I've saved that for ya."

    it "responds for non-allowed methods in existing resources" do
      conn ←
        { request: { method: POST
                   , url: "/"
                   }
        , response: { writer: testResponseWriter }
        , components: {}
        }
        # app
        # testServer
      testStatus conn `shouldEqual` Just statusMethodNotAllowed
