module Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (class EncodeJson, Json, jsonEmptyObject, (:=), (~>))
import Data.Array ((..))
import Data.Generic (class Generic)
import Data.Maybe (maybe)
import Hyper.Core (Port(Port), closeHeaders, writeStatus)
import Hyper.HTML (HTML, asString, element_, h1, li, linkTo, p, text, ul)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (respond)
import Hyper.Routing.ContentType (class MimeRender)
import Hyper.Routing.TypeLevelRouter (type (:/), type (:<|>), type (:>), Capture, linksTo, router, (:<|>))
import Hyper.Routing.TypeLevelRouter.Method (Get)
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

allPosts :: forall m. Applicative m => m (Array Post)
allPosts = pure (map (\i -> Post { id: i, title: "Post #" <> show i }) (1..10))

postsView :: forall m. Applicative m => m PostsView
postsView = PostsView <$> allPosts

viewPost :: forall m. Applicative m => PostID -> m Post
viewPost postId = pure (Post { id: postId, title: "Post #" <> show postId })

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  runServer defaultOptions onListening onRequestError {} siteRouter
  where
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

    siteRouter = router site (postsView :<|> viewPost :<|> allPosts) onRoutingError

    onRoutingError status msg =
      writeStatus status
      >=> closeHeaders
      >=> respond (maybe "" id msg)
