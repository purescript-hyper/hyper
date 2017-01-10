module Hyper.Routing.DataRouterSpec where

import Prelude
import Hyper.Method as Method
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap)
import Hyper.Core (statusNotFound, fallbackTo, class ResponseWriter, ResponseEnded, StatusLineOpen, Conn, Middleware, closeHeaders, writeStatus, statusOK)
import Hyper.HTML (p, h1, text, HTML)
import Hyper.Method (Method)
import Hyper.Response (class Response, respond)
import Hyper.Routing.DataRouter (class Addressable, class Routable, linkTo, formFor, router, POST, Route(Route), GET)
import Hyper.Test.TestServer (testStringBody, testResponseWriter, testServer)
import Node.Buffer (BUFFER)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

newtype Task = Task String

derive instance newtypeTask :: Newtype Task _

instance showTask :: Show Task where
  show = unwrap

data TestRoutes m
  = ListTasks (m ~ GET)
  | SaveTask Task (m ~ POST)

instance addressableTestRoutes :: Addressable TestRoutes where
  toPath =
    case _ of
      ListTasks _ -> []
      SaveTask _ _ -> []

instance routableGETTestRoutes :: Routable TestRoutes GET where
  fromRoute =
    case _ of
      Route [] -> Just (ListTasks id)
      _ -> Nothing

instance routablePOSTTestRoutes :: Routable TestRoutes POST where
  fromRoute =
    case _ of
      Route [] -> Just (SaveTask (Task "Be chill.") id)
      _ -> Nothing

-- The user-level routing handlerfunction, i.e. the one that responds
-- to requests for each route.
handler :: forall m method req res rw b c.
  (Monad m, Response m HTML b, ResponseWriter rw m b) =>
  TestRoutes method
  -> Middleware
  m
  (Conn { url :: String, method :: Method | req }
        { writer :: rw StatusLineOpen | res }
        c)
  (Conn { url :: String, method :: Method | req }
        { writer :: rw ResponseEnded | res }
        c)
handler =
  case _ of
    ListTasks _ ->
      writeStatus statusOK
      >=> closeHeaders
      >=> respond (formFor SaveTask (Task "Be cool"))
    SaveTask _ _ ->
      writeStatus statusOK
      >=> closeHeaders
      >=> respond (p [] [ text "Saved. "
                        , linkTo ListTasks [text "Back to tasks."]
                        ])

app :: forall m req res rw b c.
  (Monad m, Response m HTML b, ResponseWriter rw m b) =>
  Middleware
  m
  (Conn { url :: String, method :: Method | req }
        { writer :: rw StatusLineOpen | res }
        c)
  (Conn { url :: String, method :: Method | req }
        { writer :: rw ResponseEnded | res }
        c)
app =
  -- This is a little weird, but haven't found a better way yet.
  -- If it can be generically derived the user would not have to
  -- construct this record.
  router { get: handler, post: handler }
  # fallbackTo notFound
  where
    notFound =
      writeStatus statusNotFound
      >=> closeHeaders
      >=> respond (h1 [] [text "Not Found"])

spec :: forall e. Spec (buffer :: BUFFER | e) Unit
spec = do
  describe "Hyper.Routing.DataRouter" do
    let makeRequest method path =
          { request: { method: method
                     , url: path
                     }
          , response: { writer: testResponseWriter }
          , components: {}
          }
          # app
          # testServer

    it "can route a GET" do
      conn <- makeRequest Method.GET "/"
      testStringBody conn `shouldEqual` "<form method=\"POST\" action=\"/\"><input name=\"value\" value=\"Be cool\"></input></form>"

    it "can route a POST" do
      conn <- makeRequest Method.POST "/"
      testStringBody conn `shouldEqual` "<p>Saved. <a href=\"/\">Back to tasks.</a></p>"
