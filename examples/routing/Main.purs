module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Argonaut (class EncodeJson, Json, jsonEmptyObject, (:=), (~>))
import Data.Array (find, (..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Core (Port(Port), closeHeaders, writeStatus)
import Hyper.HTML (class EncodeHTML, HTML, element_, h1, li, linkTo, p, text, ul)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (contentType, respond)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture, (:<|>))
import Hyper.Routing.Links (linksTo)
import Hyper.Routing.Router (RoutingError(..), router)
import Hyper.Routing.Method (Get)
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

instance encodeHTMLPost :: EncodeHTML Post where
  encodeHTML (Post { id: postId, title}) =
    case linksTo site of
      allPostsUri :<|> _ :<|> _ ->
        element_ "section" [ h1 [] [ text title ]
                           , p [] [ text "Contents..." ]
                           , element_ "nav" [ linkTo allPostsUri [ text "All Posts" ]]
                           ]

newtype PostsView = PostsView (Array Post)

instance encodeHTMLPostsView :: EncodeHTML PostsView where
  encodeHTML (PostsView posts) =
    case linksTo site of
      _ :<|> getPostUri :<|> postsJsonUri ->
        let postLink (Post { id: postId, title }) =
              li [] [linkTo (getPostUri postId) [ text title ]]
        in element_ "section" [ h1 [] [ text "Posts" ]
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

-- Simple handlers that can access available posts in
-- the Reader monad (would likely be a database query in
-- a real app):

type AppM e a = ExceptT RoutingError (ReaderT (Array Post) (Aff e)) a

allPosts :: forall e. AppM e (Array Post)
allPosts = ask

postsView :: forall e. AppM e PostsView
postsView = PostsView <$> ask

viewPost :: forall e. PostID -> AppM e Post
viewPost postId =
  find (\(Post p) -> p.id == postId) <$> ask >>=
  case _ of
    Just post -> pure post
    -- You can throw 404 Not Found in here as well.
    Nothing -> throwError (HTTPError { status: statusNotFound
                                     , message: Just "Post not found."
                                     })


main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  runServer defaultOptions onListening onRequestError {} (siteRouter >>> flip runReaderT posts)
  where
    posts = (map (\i -> Post { id: i, title: "Post #" <> show i }) (1..10))

    siteRouter = router site (postsView :<|> viewPost :<|> allPosts) onRoutingError

    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

    onRoutingError status msg =
      writeStatus status
      >=> contentType textHTML
      >=> closeHeaders
      >=> respond (maybe "" id msg)
