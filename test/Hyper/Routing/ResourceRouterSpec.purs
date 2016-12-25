module Hyper.Routing.ResourceRouterSpec where

import Prelude
import Control.Alternative ((<|>))
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (statusMethodNotAllowed, StatusLineOpen, statusCreated, statusOK, closeHeaders, statusNotFound, writeStatus, class ResponseWriter, Conn, Middleware, ResponseEnded)
import Hyper.Method (Method(..))
import Hyper.Response (contentType, headers, respond)
import Hyper.Routing.ResourceRouter (runRouter, router, handler, resource)
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
app = runRouter notFound notAllowed (router index <|> router greetings)
  where
    notFound =
      writeStatus statusNotFound
      >=> contentType textHTML
      >=> closeHeaders
      >=> respond "Not Found"

    notAllowed method =
      writeStatus statusMethodNotAllowed
      >=> contentType textHTML
      >=> closeHeaders
      >=> respond ("Method \"" <> show method <> "\" is not allowed")

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


spec :: forall e. Spec (console :: CONSOLE | e) Unit
spec = do
  describe "Hyper.Router" do
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
      testBody conn `shouldEqual` "Hello!"

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
      testStatus conn `shouldEqual` Just (Tuple 201 "Created")
      testBody conn `shouldEqual` "OK, I've saved that for ya."

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
      testStatus conn `shouldEqual` Just (Tuple 405 "Method Not Allowed")
