module Hyper.Routing.RouterSpec (spec) where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textPlain)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Hyper.Core (class ResponseWriter, Conn, Middleware, ResponseEnded, StatusLineOpen, closeHeaders, writeStatus)
import Hyper.Method (Method(..))
import Hyper.Response (class Response, contentType, headers, respond)
import Hyper.Routing ((:<|>))
import Hyper.Routing.Router (RoutingError, router)
import Hyper.Routing.TestSite (Home(..), User(..), UserID(..), WikiPage(..), testSite)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed, statusOK)
import Hyper.Test.TestServer (testHeaders, testResponseWriter, testServer, testStatus, testStringBody)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Handler m a = ExceptT RoutingError m a

home :: forall m. Monad m => Handler m Home
home = pure Home

profile :: forall m. Monad m => UserID -> Handler m User
profile userId = pure (User userId)

friends :: forall m. Monad m => UserID -> Handler m (Array User)
friends (UserID uid) =
  pure [ User (UserID "foo")
       , User (UserID "bar")
       ]

wiki :: forall m. Monad m => Array String -> Handler m WikiPage
wiki segments = pure (WikiPage (joinWith "/" segments))

about :: forall m req res c rw rb.
         ( Monad m
         , ResponseWriter rw (ExceptT RoutingError m) rb
         , Response rb (ExceptT RoutingError m) String
         )
         => Middleware
            (ExceptT RoutingError m)
            (Conn { method :: Method, url :: String | req } { writer :: rw StatusLineOpen | res } c)
            (Conn { method :: Method, url :: String | req } { writer :: rw ResponseEnded | res } c)
about =
  writeStatus statusOK
  >=> contentType textPlain
  >=> closeHeaders
  >=> respond "This is a test."

spec :: forall e. Spec e Unit
spec =
  describe "Hyper.Routing.Router" do
    let userHandlers userId = profile userId :<|> friends userId
        handlers = home
                  :<|> userHandlers
                  :<|> wiki
                  :<|> about

        onRoutingError status msg =
          writeStatus status
          >=> headers []
          >=> respond (maybe "" id msg)

        makeRequest method path =
          { request: { method: method
                    , url: path
                    }
          , response: { writer: testResponseWriter }
          , components: {}
          }
          # (router testSite handlers onRoutingError)
          # testServer

    describe "router" do
      it "matches root" do
        conn <- makeRequest GET "/"
        testStringBody conn `shouldEqual` "<h1>Home</h1>"

      it "validates based on custom Capture instance" do
        conn <- makeRequest GET "/users/ /profile"
        testStatus conn `shouldEqual` Just statusBadRequest
        testStringBody conn `shouldEqual` "UserID must not be blank."

      it "matches nested routes" do
        conn <- makeRequest GET "/users/owi/profile"
        testStringBody conn `shouldEqual` "{\"userId\":\"owi\"}"

      it "supports arrays of JSON values" do
        conn <- makeRequest GET "/users/owi/friends"
        testStringBody conn `shouldEqual` "[{\"userId\":\"foo\"},{\"userId\":\"bar\"}]"

      it "matches CaptureAll route" do
        conn <- makeRequest GET "/wiki/foo/bar/baz.txt"
        testStringBody conn `shouldEqual` "Viewing page: foo/bar/baz.txt"

      it "matches Raw route" do
        conn <- makeRequest GET "/about"
        testHeaders conn `shouldEqual` [ Tuple "Content-Type" "text/plain" ]
        testStringBody conn `shouldEqual` "This is a test."

      it "checks HTTP method" do
        conn <- makeRequest POST "/"
        testStatus conn `shouldEqual` Just statusMethodNotAllowed
