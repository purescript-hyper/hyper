module Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Data.URI (printURI)
import Hyper.Core (Port(Port), closeHeaders, fallbackTo, statusNotFound, statusOK, writeStatus)
import Hyper.HTML (a, asString, element_, h1, li, p, text, ul)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (respond, contentType)
import Hyper.Routing.TypeLevelRouter (class FromHttpData, class ToHttpData, type (:/), type (:>), type (:<|>), (:<|>), Capture, Get, fromPathPiece, linkTo, router)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Type.Proxy (Proxy(..))

newtype PostID = PostID Int

instance fromHttpDataPostID :: FromHttpData PostID where
  fromPathPiece s = do
    n <- fromPathPiece s
    if n >= 1
      then Right (PostID n)
      else Left "Post ID must be equal to or greater than 1."

instance toHttpDataPostID :: ToHttpData PostID where
  toPathPiece (PostID n) = show n

type Archive = Get
type ShowPost = "posts" :/ Capture "id" PostID :> Get
type App = Archive :<|> ShowPost

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  router (Proxy :: Proxy App) (renderArchive :<|> renderPost)
  # fallbackTo notFound
  # runServer defaultOptions onListening onRequestError {}

  where
    renderArchive =
      htmlWithStatus statusOK $
      element_ "section" [ h1 [] [ text "Archive" ]
                         , ul [] (map postLink (1..10))
                         ]

    -- Type-safe linking

    postLink n =
      let uri = (linkTo (Proxy :: Proxy ShowPost) (PostID n))
      in li [] [a [Tuple "href" (printURI uri)] [text ("Post #" <> show n)]]

    homeLink =
      a [ Tuple "href" (printURI (linkTo (Proxy :: Proxy Archive)))] [ text "Home" ]

    renderPost (PostID pId) =
      htmlWithStatus statusOK $
      element_ "section" [ h1 [] [ text ("Post " <> show pId) ]
                         , p [] [ text "Lorem whatever..." ]
                         , p [] [ homeLink ]
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
