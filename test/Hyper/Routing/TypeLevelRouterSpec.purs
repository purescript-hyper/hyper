module Hyper.Routing.TypeLevelRouterSpec (spec) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.URI (printURI)
import Hyper.Routing.TypeLevelRouter (class FromHttpData, class ToHttpData, type (:<|>), type (:>), Capture, Get, Handler(..), Lit, RoutingError(..), fromPathPiece, linkTo, runRouter, (:<|>))
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

type Root = Lit "html" :> Get "text/html"
type GetPost = Lit "posts" :> Capture "id" PostID :> Get "text/plain"
type UserRoutes = Lit "users" :> Capture "user-id" UserID :> (Lit "profile" :> Get "text/plain"
                                                              :<|> Lit "settings" :> Get "text/plain")

type TestAPI =
  Root
  :<|> GetPost
  :<|> UserRoutes

request :: String -> String -> Either RoutingError String
request method url =
  runRouter (Proxy :: Proxy TestAPI) (renderHtml
                                      :<|> renderPost
                                      :<|> userHandlers
                                     ) method url
  where
    renderHtml = Handler (const "<h1>HTML</h1>")
    renderPost (PostID n) = Handler (\_ -> "Post #" <> show n)

    userHandlers i = renderProfile i :<|> renderSettings i
    renderProfile (UserID s) = Handler (\_ -> "Profile of " <> s)
    renderSettings (UserID s) = Handler (\_ -> "Settings of " <> s)

spec :: forall e. Spec e Unit
spec =
  describe "Hyper.Routing.TypeLevelRouter" do
    describe "links" do
      it "returns link for Lit" $
        printURI (linkTo (Proxy :: Proxy Root)) `shouldEqual` "/html/"

      it "returns link for Lit and Capture" $
        printURI (linkTo (Proxy :: Proxy GetPost) (PostID 10)) `shouldEqual` "/posts/10/"

    describe "route" do
      it "matches Lit" $
        request "GET" "/html" `shouldEqual` Right "<h1>HTML</h1>"

      it "matches custom Capture" $
        request "GET" "/posts/123/" `shouldEqual` Right "Post #123"

      it "validates based on customer Capture instance" $
        request "GET" "/posts/0/" `shouldEqual` Left (HTTPError 400 (Just "PostID must be equal to or greater than 1."))

      it "matches nested routes" $
        request "GET" "/users/owi/profile/" `shouldEqual` Right "Profile of owi"
