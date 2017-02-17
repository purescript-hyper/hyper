module Examples.Authentication where

import Prelude
import Hyper.Node.BasicAuth as BasicAuth
import Control.IxMonad ((:>>=), (:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(Just, Nothing))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(Tuple))
import Hyper.HTML (asString, p, text)
import Hyper.Middleware.Class (getConn)
import Hyper.Node.Server (runServer, defaultOptions)
import Hyper.Port (Port(..))
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusOK)
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

    myProfilePage =
      getConn :>>= \conn ->
      case conn.components.authentication of
        User name → do
          writeStatus statusOK
          :*> contentType textHTML
          :*> closeHeaders
          :*> respond (asString (p [] [text ("You are authenticated as " <> name <> ".")]))

    app = do
      BasicAuth.withAuthentication userFromBasicAuth
      :*> BasicAuth.authenticated "Authentication Example" myProfilePage
    components = { authentication: unit }
  in runServer defaultOptions onListening onRequestError components app
