module Hyper.Routing.RouterSpec (spec) where

import Prelude
import Data.StrMap as StrMap
import Control.IxMonad (ibind)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textPlain)
import Data.StrMap (StrMap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware)
import Hyper.Response (class Response, contentType, headers, respond, class ResponseWriter, ResponseEnded, StatusLineOpen, closeHeaders, writeStatus)
import Hyper.Routing ((:<|>))
import Hyper.Routing.Router (router)
import Hyper.Routing.TestSite (Home(..), User(..), UserID(..), WikiPage(..), testSite)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed, statusOK)
import Hyper.Test.TestServer (TestResponseWriter(..), testHeaders, testServer, testStatus, testStringBody)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

home :: forall m. Monad m => m Home
home = pure Home

profile :: forall m. Monad m => UserID -> m User
profile userId = pure (User userId)

friends :: forall m. Monad m => UserID -> m (Array User)
friends (UserID uid) =
  pure [ User (UserID "foo")
       , User (UserID "bar")
       ]

wiki :: forall m. Monad m => Array String -> m WikiPage
wiki segments = pure (WikiPage (joinWith "/" segments))

about :: forall m req res c rw rb.
         ( Monad m
         , ResponseWriter rw m rb
         , Response rb m String
         )
         => Middleware
            m
            (Conn { method :: Either Method CustomMethod, url :: String | req } { writer :: rw StatusLineOpen | res } c)
            (Conn { method :: Either Method CustomMethod, url :: String | req } { writer :: rw ResponseEnded | res } c)
            Unit
about = do
  writeStatus statusOK
  contentType textPlain
  closeHeaders
  respond "This is a test."
  where bind = ibind

spec :: forall e. Spec e Unit
spec =
  describe "Hyper.Routing.Router" do
    let userHandlers userId = profile userId :<|> friends userId
        handlers = home
                   :<|> userHandlers
                   :<|> wiki
                   :<|> about

        onRoutingError status msg = do
          writeStatus status
          headers []
          respond (maybe "" id msg)
          where bind = ibind

        makeRequestWithHeaders method path headers =
          let conn = { request: { method: Left method
                                , url: path
                                , headers: headers
                                }
                     , response: { writer: TestResponseWriter }
                     , components: {}
                     }
              app = router testSite handlers onRoutingError
          in evalMiddleware app conn
             # testServer

        makeRequest method path =
          makeRequestWithHeaders method path (StrMap.empty :: StrMap String)

    describe "router" do
      it "matches root" do
        conn <- makeRequest GET "/"
        testStringBody conn `shouldEqual` "<h1>Home</h1>"

      it "considers Accept header for multi-content-type resources" do
        conn <- makeRequestWithHeaders GET "/" (StrMap.singleton "accept" "application/json")
        testStatus conn `shouldEqual` Just statusOK
        testStringBody conn `shouldEqual` "{}"

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
        testStringBody conn `shouldEqual` "Viewing page: foo&#x2F;bar&#x2F;baz.txt"

      it "matches Raw route" do
        conn <- makeRequest GET "/about"
        testHeaders conn `shouldEqual` [ Tuple "Content-Type" "text/plain" ]
        testStringBody conn `shouldEqual` "This is a test."

      it "checks HTTP method" do
        conn <- makeRequest POST "/"
        testStatus conn `shouldEqual` Just statusMethodNotAllowed
