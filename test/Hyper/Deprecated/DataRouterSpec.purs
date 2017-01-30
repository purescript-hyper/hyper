module Hyper.Deprecated.DataRouterSpec where

import Prelude
import Data.Int as Int
import Hyper.Method as Method
import Data.Array (singleton)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (fallbackTo, class ResponseWriter, ResponseEnded, StatusLineOpen, Conn, Middleware, closeHeaders, writeStatus)
import Hyper.HTML (asString, h1, li, text, ul)
import Hyper.Method (Method)
import Hyper.Response (class Response, respond)
import Hyper.Deprecated.DataRouter (linkTo, redirectTo, class Addressable, class Routable, formFor, router, POST, Route(Route), GET)
import Hyper.Status (statusFound, statusNotFound, statusOK)
import Hyper.Test.TestServer (testHeaders, testStatus, testStringBody, testResponseWriter, testServer)
import Node.Buffer (BUFFER)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

newtype TaskId = TaskId Int

derive instance newtypeTaskId :: Newtype TaskId _

instance showTaskId :: Show TaskId where
  show = show <<< unwrap

data TestRoutes m
  = ListTasks (m ~ GET)
  | NewTask (m ~ GET)
  | CreateTask (m ~ POST)
  | ShowTask TaskId (m ~ GET)
  | UpdateTask TaskId (m ~ POST)

instance addressableTestRoutes :: Addressable TestRoutes where
  toPath =
    case _ of
      ListTasks _ -> ["tasks"]
      NewTask _ -> ["tasks"]
      CreateTask _ -> ["tasks"]
      ShowTask taskId _ -> ["tasks", show taskId]
      UpdateTask taskId _ -> ["tasks", show taskId]

instance routableGETTestRoutes :: Routable TestRoutes GET where
  fromRoute =
    case _ of
      Route ["tasks"] -> Just (ListTasks id)
      Route ["tasks", "new"] -> Just (NewTask id)
      Route ["tasks", s] -> ShowTask <$> (TaskId <$> Int.fromString s) <*> pure id
      _ -> Nothing

instance routablePOSTTestRoutes :: Routable TestRoutes POST where
  fromRoute =
    case _ of
      Route ["tasks"] -> Just (CreateTask id)
      Route ["tasks", s] -> UpdateTask <$> (TaskId <$> Int.fromString s) <*> pure id
      _ -> Nothing

-- The user-level routing handler function, i.e. the one that responds
-- to requests for each route.
--
-- NOTE: It's a bit shaky that it can deconstruct the ListTasks and
-- SaveTask constructors, regardless of their differently typed Leibniz
-- arguments, but it seems OK as the Routable instance cannot incorrectly
-- route POST to GET and vice versa.
handler :: forall m method req res rw b c.
  (Monad m, Response b m String, ResponseWriter rw m b) =>
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
      let makeTaskLink taskId = linkTo (ShowTask (TaskId taskId)) [text ("Task #" <> show taskId)]
      in writeStatus statusOK
         >=> closeHeaders
         >=> respond (asString (ul [] (map (li [] <<< singleton <<< makeTaskLink) [1, 2, 3])))
    NewTask _ ->
      writeStatus statusOK
      >=> closeHeaders
      >=> respond (asString (formFor (CreateTask) []))
    ShowTask taskId _ ->
      writeStatus statusOK
      >=> closeHeaders
      >=> respond (asString (h1 [] [text ("Task #" <> show taskId)]))
    CreateTask _ ->
      -- Let's pretend this saved the Task in a database.
      let createdId = TaskId 4
      in redirectTo (ShowTask createdId)
    UpdateTask taskId _ ->
      -- Let's pretend this updated the Task in a database.
      redirectTo (ShowTask taskId)

app :: forall m req res rw b c.
  (Monad m, Response b m String, ResponseWriter rw m b) =>
  Middleware
  m
  (Conn { url :: String, method :: Method | req }
        { writer :: rw StatusLineOpen | res }
        c)
  (Conn { url :: String, method :: Method | req }
        { writer :: rw ResponseEnded | res }
        c)
app =
  -- This record is a little weird, but haven't found any better way
  -- yet (the only other approach I could come up with was type classes,
  -- and that would results in massive type signatures as one parameter
  -- would be the middleware type).
  --
  -- I hope that if the record it can be generically derived, then the user
  -- will not have to construct it, only provide the handler function.
  router { get: handler, post: handler }
  # fallbackTo notFound
  where
    notFound =
      writeStatus statusNotFound
      >=> closeHeaders
      >=> respond (asString (h1 [] [text "Not Found"]))

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

    it "lists tasks" do
      conn <- makeRequest Method.GET "/tasks"
      testStringBody conn `shouldEqual`
        ("<ul>"
         <> "<li><a href=\"/tasks/1\">Task #1</a></li>"
         <> "<li><a href=\"/tasks/2\">Task #2</a></li>"
         <> "<li><a href=\"/tasks/3\">Task #3</a></li>"
         <> "</ul>")

    it "show new task form" do
      conn <- makeRequest Method.GET "/tasks/new"
      testStringBody conn `shouldEqual` "<form method=\"POST\" action=\"/tasks\"></form>"

    it "creates a new task" do
      conn <- makeRequest Method.POST "/tasks"
      testStatus conn `shouldEqual` Just statusFound
      testHeaders conn `shouldEqual` [Tuple "Location" "/tasks/4"]

    it "show a specific task" do
      conn <- makeRequest Method.GET "/tasks/1"
      testStringBody conn `shouldEqual` "<h1>Task #1</h1>"

    it "updates a task" do
      conn <- makeRequest Method.POST "/tasks/2"
      testStatus conn `shouldEqual` Just statusFound
      testHeaders conn `shouldEqual` [Tuple "Location" "/tasks/2"]
