module Hyper.Routing.ContentType where

import Data.Argonaut (Json)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Function ((<<<))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Hyper.HTML (HTML)
import Hyper.HTML (asString) as HTML
import Type.Proxy (Proxy)

class HasMediaType ct where
  getMediaType :: Proxy ct -> MediaType

class MimeRender a ct b | a -> b  where
  mimeRender :: Proxy ct -> a -> b

instance hasMediaTypeJson :: HasMediaType Json where
  getMediaType _ = applicationJSON


instance mimeRenderJson :: EncodeJson a => MimeRender a Json String where
  mimeRender _ = stringify <<< encodeJson


instance hasMediaTypeTextHTML :: HasMediaType HTML where
  getMediaType _ = textHTML

instance mimeRenderTextHTML :: MimeRender HTML HTML String where
  mimeRender p = HTML.asString
