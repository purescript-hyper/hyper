module Examples.Authentication where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Maybe (Maybe(Just, Nothing))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(Tuple))
import Hyper.Middleware.Class (getConn)
import Hyper.Node.BasicAuth as BasicAuth
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusOK)
import Text.Smolder.HTML (p)
import Text.Smolder.Markup (text)
import Text.Smolder.Renderer.String (render)

data User = User String

-- This could be a function checking the username/password in a database.
userFromBasicAuth :: Tuple String String -> Aff (Maybe User)
userFromBasicAuth =
  case _ of
    Tuple "admin" "admin" -> pure (Just (User "Administrator"))
    _ -> pure Nothing

main :: Effect Unit
main =
  let
    myProfilePage = Ix.do
      conn <- getConn
      case conn.components.authentication of
        User name → Ix.do
          writeStatus statusOK
          contentType textHTML
          closeHeaders
          respond (render (p (text ("You are authenticated as " <> name <> "."))))

    app = Ix.do
      BasicAuth.withAuthentication userFromBasicAuth
      BasicAuth.authenticated "Authentication Example" myProfilePage
    components = { authentication: unit }
  in runServer defaultOptionsWithLogging components app
