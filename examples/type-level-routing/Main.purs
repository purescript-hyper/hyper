module Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.MediaType.Common (textHTML)
import Hyper.Core (Port(Port), closeHeaders, fallbackTo, statusNotFound, statusOK, writeStatus)
import Hyper.HTML (asString, element_, h1, li, linkTo, p, text, ul)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (respond, contentType)
import Hyper.Routing.PathPiece (class FromPathPiece, class ToPathPiece, fromPathPiece, toPathPiece)
import Hyper.Routing.TypeLevelRouter (type (:/), type (:<|>), type (:>), Capture, Get, linksTo, router, (:<|>))
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Type.Proxy (Proxy(..))

newtype PostID = PostID Int

instance fromPathPiecePostID :: FromPathPiece PostID where
  fromPathPiece s = do
    n <- fromPathPiece s
    if n >= 1
      then Right (PostID n)
      else Left "Post ID must be equal to or greater than 1."

instance toPathPiecePostID :: ToPathPiece PostID where
  toPathPiece (PostID n) = toPathPiece n

type Site = Get :<|> "posts" :/ Capture "id" PostID :> Get

site :: Proxy Site
site = Proxy

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main = app (linksTo site)
  where
    -- Automatic type-safe link functions!
    app (archiveURI :<|> postURI) =

      router (Proxy :: Proxy Site) (renderArchive :<|> renderPost)
      # fallbackTo notFound
      # runServer defaultOptions onListening onRequestError {}

      where
        renderArchive =
          htmlWithStatus statusOK $
          element_ "section" [ h1 [] [ text "Archive" ]
                             , ul [] (map postLink (1..10))
                             ]

        postLink n = li [] [linkTo (postURI (PostID n)) [ text ("Post #" <> show n) ]]

        renderPost (PostID pId) =
          htmlWithStatus statusOK $
          element_ "section" [ h1 [] [ text ("Post " <> show pId) ]
                             , p [] [ text "Lorem whatever..." ]
                             , p [] [ linkTo archiveURI [ text "Archive" ] ]
                             ]

        notFound =
          htmlWithStatus statusNotFound $
          element_ "section" [ h1 [] [ text "Not Found!" ]
                             , p [] [ text "The resource you requested does not exist." ]
                             ]

        onListening (Port port) = log ("Listening on http://localhost:" <> show port)
        onRequestError err = log ("Request failed: " <> show err)

        htmlWithStatus status doc =
          writeStatus status
          >=> contentType textHTML
          >=> closeHeaders
          >=> respond (asString doc)
