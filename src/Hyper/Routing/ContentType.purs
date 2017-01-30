module Hyper.Routing.ContentType where

import Data.Argonaut (Json)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Function ((<<<))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Hyper.HTML (class EncodeHTML, HTML, encodeHTML)
import Hyper.HTML (asString) as HTML
import Type.Proxy (Proxy)

class HasMediaType ct where
  getMediaType :: Proxy ct -> MediaType

class MimeRender a ct b | a -> b, ct -> b  where
  mimeRender :: Proxy ct -> a -> b

instance hasMediaTypeJson :: HasMediaType Json where
  getMediaType _ = applicationJSON


instance mimeRenderJson :: EncodeJson a => MimeRender a Json String where
  mimeRender _ = stringify <<< encodeJson


instance hasMediaTypeHTML :: HasMediaType HTML where
  getMediaType _ = textHTML

instance mimeRenderHTML :: MimeRender HTML HTML String where
  mimeRender p = HTML.asString

mimeRenderHtml :: forall a. MimeRender a HTML HTML => Proxy HTML -> a -> String
mimeRenderHtml p = HTML.asString <<< mimeRender p

instance mimeRenderHTMLEncodeHTML :: EncodeHTML a => MimeRender a HTML String where
  mimeRender _ = HTML.asString <<< encodeHTML
