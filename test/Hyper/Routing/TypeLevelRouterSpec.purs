module Hyper.Routing.TypeLevelRouterSpec (spec) where

import Prelude
import Data.Argonaut (class EncodeJson, Json, jsonEmptyObject, (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith, trim)
import Data.URI (printURI)
import Hyper.Core (writeStatus)
import Hyper.HTML (HTML, asString, h1, text)
import Hyper.Method (Method(..))
import Hyper.Response (headers, respond)
import Hyper.Routing.ContentType (class MimeRender)
import Hyper.Routing.PathPiece (class FromPathPiece, class ToPathPiece)
import Hyper.Routing.TypeLevelRouter (type (:/), type (:<|>), type (:>), Capture, CaptureAll, linksTo, router, (:<|>))
import Hyper.Routing.TypeLevelRouter.Method (Get)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed)
import Hyper.Test.TestServer (testResponseWriter, testServer, testStatus, testStringBody)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

data Home = Home

instance mimeRenderHome :: MimeRender Home HTML String where
  mimeRender _ Home = asString (h1 [] [text "Home"])

newtype UserID = UserID String

instance fromPathPieceUserID :: FromPathPiece UserID where
  fromPathPiece s =
    case trim s of
      "" -> Left "UserID must not be blank."
      s' -> Right (UserID s')

instance toPathPieceUserID :: ToPathPiece UserID where
  toPathPiece (UserID s) = s

data User = User UserID

instance encodeUser :: EncodeJson User where
  encodeJson (User (UserID userId)) =
    "userId" := userId
    ~> jsonEmptyObject

data WikiPage = WikiPage String

instance mimeRenderWikiPage :: MimeRender WikiPage HTML String where
  mimeRender _ (WikiPage title) = asString (text ("Viewing page: " <> title))

type TestAPI =
  Get HTML Home
  -- nested routes with capture
  :<|> "users" :/ Capture "user-id" UserID :> ("profile" :/ Get Json User
                                               :<|> "friends" :/ Get Json (Array User))
  -- capture all
  :<|> "wiki" :/ CaptureAll "segments" String :> Get HTML WikiPage

testSite :: Proxy TestAPI
testSite = Proxy

home :: Home
home = Home

profile :: UserID -> User
profile userId = User userId

friends :: UserID -> Array User
friends (UserID uid) = [ User (UserID "foo")
                       , User (UserID "bar")
                       ]

wiki :: Array String -> WikiPage
wiki segments = WikiPage (joinWith "/" segments)

spec :: forall e. Spec e Unit
spec = do
  let userHandlers userId = profile userId :<|> friends userId
      handlers = home
                 :<|> userHandlers
                 :<|> wiki

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

  describe "Hyper.Routing.TypeLevelRouter" do
    pure unit

    describe "links" $

      case linksTo testSite of
        (homeUri :<|> userLinks :<|> wikiUri) -> do

          it "returns link for Lit" $
            printURI homeUri `shouldEqual` "/"

          it "returns link for nested routes" $
            case userLinks (UserID "owi") of
              (profileUri :<|> friendsUri) -> do
                  printURI profileUri `shouldEqual` "/users/owi/profile"
                  printURI friendsUri `shouldEqual` "/users/owi/friends"
          it "returns link for CaptureAll" $
            printURI (wikiUri ["foo", "bar", "baz.txt"]) `shouldEqual` "/wiki/foo/bar/baz.txt"

    describe "route" do
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

      it "checks HTTP method" do
        conn <- makeRequest POST "/"
        testStatus conn `shouldEqual` Just statusMethodNotAllowed
