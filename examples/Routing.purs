module Examples.Routing where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class EncodeJson, gEncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Array (find, (..))
import Data.Foldable (traverse_)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture, (:<|>))
import Hyper.Routing.ContentType.HTML (class EncodeHTML, HTML, linkTo)
import Hyper.Routing.ContentType.JSON (JSON)
import Hyper.Routing.Links (linksTo)
import Hyper.Routing.Method (Get)
import Hyper.Routing.Router (RoutingError(..), router)
import Hyper.Status (statusNotFound)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (h1, li, nav, p, section, ul)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))

type PostID = Int

newtype Post = Post { id :: PostID
                    , title :: String
                    }

derive instance genericPost :: Generic Post

instance encodeJsonPost :: EncodeJson Post where
  encodeJson (Post { id, title }) =
    "id" := id
    ~> "title" := title
    ~> jsonEmptyObject

instance encodeHTMLPost :: EncodeHTML Post where
  encodeHTML (Post { id: postId, title}) =
    case linksTo site of
      allPostsUri :<|> _ ->
        section do
          h1 (text title)
          p (text "Contents...")
          nav (linkTo allPostsUri (text "All Posts"))

newtype PostsView = PostsView (Array Post)

derive instance genericPostsView :: Generic PostsView

instance encodeJsonPostsView :: EncodeJson PostsView where
  encodeJson = gEncodeJson

instance encodeHTMLPostsView :: EncodeHTML PostsView where
  encodeHTML (PostsView posts) =
    case linksTo site of
      _ :<|> getPostUri ->
        let postLink (Post { id: postId, title }) =
              li (linkTo (getPostUri postId) (text title))
        in section do
            h1 (text "Posts")
            ul (traverse_ postLink posts)

type Site = Get (HTML :<|> JSON) PostsView
            :<|> "posts" :/ Capture "id" PostID :> Get (HTML :<|> JSON) Post

site :: Proxy Site
site = Proxy

type AppM e a = ExceptT RoutingError (Aff e) a

-- This would likely be a database query in
-- a real app:
allPosts :: forall e. AppM e (Array Post)
allPosts = pure (map (\i -> Post { id: i, title: "Post #" <> show i }) (1..10))

postsView :: forall e. AppM e PostsView
postsView = PostsView <$> allPosts

viewPost :: forall e. PostID -> AppM e Post
viewPost postId =
  find (\(Post p) -> p.id == postId) <$> allPosts >>=
  case _ of
    Just post -> pure post
    -- You can throw 404 Not Found in here as well.
    Nothing -> throwError (HTTPError { status: statusNotFound
                                     , message: Just "Post not found."
                                     })

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR | e) Unit
main =
  runServer defaultOptions {} siteRouter
  where
    siteRouter = router site (postsView :<|> viewPost) onRoutingError
    onRoutingError status msg = do
      writeStatus status
      :*> contentType textHTML
      :*> closeHeaders
      :*> respond (maybe "" id msg)
