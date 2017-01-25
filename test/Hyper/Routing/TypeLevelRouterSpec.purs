module Hyper.Routing.TypeLevelRouterSpec (spec) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith, trim)
import Data.URI (printURI)
import Hyper.Core (closeHeaders, fallbackTo, statusBadRequest, statusNotFound, statusOK, writeStatus)
import Hyper.Method (Method(..))
import Hyper.Response (headers, respond)
import Hyper.Routing.TypeLevelRouter (class FromHttpData, class ToHttpData, type (:/), type (:<|>), type (:>), Capture, CaptureAll, Get, fromPathPiece, linksTo, router, (:<|>))
import Hyper.Test.TestServer (testResponseWriter, testServer, testStatus, testStringBody)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

newtype PostID = PostID Int

instance fromHttpDataPostID :: FromHttpData PostID where
  fromPathPiece s = do
    n <- fromPathPiece s
    if n >= 1
      then Right (PostID n)
      else Left "PostID must be equal to or greater than 1."

instance toHttpDataPostID :: ToHttpData PostID where
  toPathPiece (PostID n) = show n

newtype UserID = UserID String

instance fromHttpDataUserID :: FromHttpData UserID where
  fromPathPiece s =
    case trim s of
      "" -> Left "UserID cannot be blank."
      s' -> Right (UserID s')

instance toHttpDataUserID :: ToHttpData UserID where
  toPathPiece (UserID s) = s

type TestAPI =
  Get
  :<|> "posts" :/ Capture "id" PostID :> Get
  -- nested routes with capture
  :<|> "users" :/ Capture "user-id" UserID :> ("profile" :/ Get :<|> "settings" :/ Get)
  -- capture all
  :<|> "wiki" :/ CaptureAll "segments" String :> Get

testApi :: Proxy TestAPI
testApi = Proxy

spec :: forall e. Spec e Unit
spec = do
  let renderHtml =
        writeStatus statusOK
        >=> headers []
        >=> respond "<h1>HTML</h1>"

      renderPost (PostID n) =
        writeStatus statusOK
        >=> headers []
        >=> respond ("Post #" <> show n)

      userHandlers userId = renderProfile userId :<|> renderSettings userId

      renderProfile (UserID s) =
        writeStatus statusOK
        >=> headers []
        >=> respond ("Profile of " <> s)

      renderSettings (UserID s) =
        writeStatus statusOK
        >=> headers []
        >=> respond ("Settings of " <> s)

      renderWiki segments =
        writeStatus statusOK
        >=> closeHeaders
        >=> respond ("Viewing file: " <> joinWith "/" segments)

      handlers = renderHtml :<|> renderPost :<|> userHandlers :<|> renderWiki

      notFound =
        writeStatus statusNotFound
        >=> headers []
        >=> respond "Not Found"

      makeRequest method path =
        { request: { method: method
                   , url: path
                   }
        , response: { writer: testResponseWriter }
        , components: {}
        }
        # (router testApi handlers # fallbackTo notFound)
        # testServer

  describe "Hyper.Routing.TypeLevelRouter" do

    describe "links" $

      case linksTo testApi of
        (root :<|> getPost :<|> userRoutes :<|> wiki) -> do

          it "returns link for Lit" $
            printURI root `shouldEqual` "/"

          it "returns link for Lit and Capture" $
            printURI (getPost (PostID 10)) `shouldEqual` "/posts/10/"

          it "returns link for nested routes" $
            case userRoutes (UserID "owi") of
              (profile :<|> settings) -> do
                  printURI profile `shouldEqual` "/users/owi/profile/"
                  printURI settings `shouldEqual` "/users/owi/settings/"

    describe "route" do
      it "matches root" do
        conn <- makeRequest GET "/"
        testStringBody conn `shouldEqual` "<h1>HTML</h1>"

      it "matches custom Capture" do
        conn <- makeRequest GET "/posts/123/"
        testStringBody conn `shouldEqual` "Post #123"

      it "validates based on customer Capture instance" do
        conn <- makeRequest GET "/posts/0/"
        testStatus conn `shouldEqual` Just statusBadRequest
        testStringBody conn `shouldEqual` "PostID must be equal to or greater than 1."

      it "matches nested routes" do
        conn <- makeRequest GET "/users/owi/profile/"
        testStringBody conn `shouldEqual` "Profile of owi"

      it "matches CaptureAll route" do
        conn <- makeRequest GET "/wiki/foo/bar/baz.txt"
        testStringBody conn `shouldEqual` "Viewing file: foo/bar/baz.txt"
