module Hyper.Routing.ContentType where

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Function ((<<<))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Type.Proxy (Proxy)

import Hyper.HTML (HTML)
import Hyper.HTML (asString) as HTML

class Accept ct where
  contentTypes :: Proxy ct -> Array MediaType

class Accept ct <= MimeRender ct a b where
  mimeRender :: Proxy ct -> a -> b

data JSON
data TextHTML

instance acceptJSON :: Accept JSON where
  contentTypes _ = [applicationJSON]

instance mimeRenderJSON :: EncodeJson a => MimeRender JSON a String where
  mimeRender _ = stringify <<< encodeJson

instance acceptTextHTML :: Accept TextHTML where
  contentTypes _ = [textHTML]

instance mimeRenderTextHTMLString :: MimeRender TextHTML a HTML
                                     => MimeRender TextHTML a String where
  mimeRender p = HTML.asString <<< mimeRender p
