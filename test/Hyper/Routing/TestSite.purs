module Hyper.Routing.TestSite where

import Prelude
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Either (Either(..))
import Data.String (trim)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture, CaptureAll, Raw)
import Hyper.Routing.ContentType.HTML (HTML, class EncodeHTML)
import Hyper.Routing.ContentType.JSON (JSON)
import Hyper.Routing.Method (Get)
import Hyper.Routing.PathPiece (class FromPathPiece, class ToPathPiece)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))

data Home = Home

instance encodeJsonHome :: EncodeJson Home where
  encodeJson Home = jsonEmptyObject

instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home = h1 (text "Home")

newtype UserID = UserID String

instance fromPathPieceUserID :: FromPathPiece UserID where
  fromPathPiece s =
    case trim s of
      "" -> Left "UserID must not be blank."
      s' -> Right (UserID s')

instance toPathPieceUserID :: ToPathPiece UserID where
  toPathPiece (UserID s) = s

data User = User UserID

instance encodeUser :: EncodeJson User where
  encodeJson (User (UserID userId)) =
    "userId" := userId
    ~> jsonEmptyObject

data WikiPage = WikiPage String

instance encodeHTMLWikiPage :: EncodeHTML WikiPage where
  encodeHTML (WikiPage title) = text ("Viewing page: " <> title)

type TestSite =
  Get (HTML :<|> JSON) Home
  -- nested routes with capture
  :<|> "users" :/ Capture "user-id" UserID :> ("profile" :/ Get JSON User
                                               :<|> "friends" :/ Get JSON (Array User))
  -- capture all
  :<|> "wiki" :/ CaptureAll "segments" String :> Get HTML WikiPage
  -- raw middleware
  :<|> "about" :/ Raw "GET"

testSite :: Proxy TestSite
testSite = Proxy
