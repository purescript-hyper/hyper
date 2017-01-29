module Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class EncodeJson, Json, jsonEmptyObject, (:=), (~>))
import Data.Array (find, (..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Core (Port(Port), closeHeaders, writeStatus)
import Hyper.HTML (HTML, asString, element_, h1, li, linkTo, p, text, ul)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (contentType, respond)
import Hyper.Routing.ContentType (class MimeRender)
import Hyper.Routing.TypeLevelRouter (type (:/), type (:<|>), type (:>), Capture, RoutingError(..), linksTo, router, (:<|>))
import Hyper.Routing.TypeLevelRouter.Method (Get)
import Hyper.Status (statusNotFound)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Type.Proxy (Proxy(..))

type PostID = Int

newtype Post = Post { id :: PostID
                    , title :: String
                    }

derive instance genericPost :: Generic Post

instance encodePost :: EncodeJson Post where
  encodeJson (Post { id, title }) =
    "id" := id
    ~> "title" := title
    ~> jsonEmptyObject

instance mimeRenderPost :: MimeRender Post HTML String where
  mimeRender _ (Post { id: postId, title}) =
    case linksTo site of
      allPostsUri :<|> _ :<|> _ ->
        asString $
        element_ "section" [ h1 [] [ text title ]
                           , p [] [ text "Contents..." ]
                           , element_ "nav" [ linkTo allPostsUri [ text "All Posts" ]]
                           ]

newtype PostsView = PostsView (Array Post)

instance mimeRenderPostsView :: MimeRender PostsView HTML String where
  mimeRender _ (PostsView posts) =
    case linksTo site of
      _ :<|> getPostUri :<|> postsJsonUri ->
        let postLink (Post { id: postId, title }) =
              li [] [linkTo (getPostUri postId) [ text title ]]
        in asString $
           element_ "section" [ h1 [] [ text "Posts" ]
                              , ul [] (map postLink posts)
                              , p [] [ text "Get posts as "
                                     , linkTo postsJsonUri [ text "JSON" ]
                                     ]
                              ]

type Site = Get HTML PostsView
            :<|> "posts" :/ Capture "id" PostID :> Get HTML Post
            :<|> "posts.json" :/ Get Json (Array Post)

site :: Proxy Site
site = Proxy

-- Simple handlers that are given all available posts
-- (would be a database in a real app):

allPosts :: forall m. Monad m =>
            Array Post
         -> ExceptT RoutingError m (Array Post)
allPosts = pure

postsView :: forall m. Monad m =>
             Array Post
          -> ExceptT RoutingError m PostsView
postsView = pure <<< PostsView

viewPost :: forall m. Monad m =>
            Array Post
         -> PostID
         -> ExceptT RoutingError m Post
viewPost posts postId = do
  case find (\(Post p) -> p.id == postId) posts of
    Just post -> pure post
    -- You can throw 404 Not Found in here as well.
    Nothing -> throwError (HTTPError { status: statusNotFound
                                     , message: Just "Post not found."
                                     })


main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  runServer defaultOptions onListening onRequestError {} siteRouter
  where
    posts = (map (\i -> Post { id: i, title: "Post #" <> show i }) (1..10))

    siteRouter = router site (postsView posts :<|> viewPost posts :<|> allPosts posts) onRoutingError

    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

    onRoutingError status msg =
      writeStatus status
      >=> contentType textHTML
      >=> closeHeaders
      >=> respond (maybe "" id msg)
