module Main where

import Prelude
import Hyper.Node.BasicAuth as BasicAuth
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(Just, Nothing))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (writeStatus, closeHeaders, statusOK, Port(Port))
import Hyper.HTML.DSL (p, text, html)
import Hyper.Node.Server (runServer, defaultOptions)
import Hyper.Response (contentType)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)

data User = User String

-- This could be a function checking the username/password in a database.
userFromBasicAuth :: forall e. Tuple String String -> Aff e (Maybe User)
userFromBasicAuth =
  case _ of
    Tuple "admin" "admin" -> pure (Just (User "Administrator"))
    _ -> pure Nothing

main :: forall e. Eff (console :: CONSOLE, http ∷ HTTP, buffer :: BUFFER | e) Unit
main =
  let
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

    myProfilePage conn@{ components: { authentication: User name } } =
      writeStatus statusOK conn
      >>= contentType textHTML
      >>= closeHeaders
      >>= html (p [] (text ("You are authenticated as " <> name <> ".")))

    app = BasicAuth.withAuthentication userFromBasicAuth
          >=> BasicAuth.authenticated "Authentication Example" myProfilePage
    components = { authentication: unit }
  in runServer defaultOptions onListening onRequestError components app
