module Hyper.Routing.TestSite where

import Prelude
import Data.Argonaut (class EncodeJson, Json, jsonEmptyObject, (:=), (~>))
import Data.Either (Either(..))
import Data.String (trim)
import Hyper.HTML (class EncodeHTML, HTML, h1, text)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture, CaptureAll, Raw)
import Hyper.Routing.Method (Get)
import Hyper.Routing.PathPiece (class FromPathPiece, class ToPathPiece)
import Type.Proxy (Proxy(..))

data Home = Home

instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home = h1 [] [text "Home"]

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
  Get HTML Home
  -- nested routes with capture
  :<|> "users" :/ Capture "user-id" UserID :> ("profile" :/ Get Json User
                                               :<|> "friends" :/ Get Json (Array User))
  -- capture all
  :<|> "wiki" :/ CaptureAll "segments" String :> Get HTML WikiPage
  -- raw middleware
  :<|> "about" :/ Raw "GET"

testSite :: Proxy TestSite
testSite = Proxy
