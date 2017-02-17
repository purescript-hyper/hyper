module Hyper.Routing.ContentType.JSON where

import Prelude
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Hyper.Routing.ContentType (class AllMimeRender, class HasMediaType, class MimeRender, getMediaType, mimeRender)

data JSON

instance hasMediaTypeJson :: HasMediaType JSON where
  getMediaType _ = applicationJSON

instance mimeRenderJson :: EncodeJson a => MimeRender a JSON String where
  mimeRender _ = stringify <<< encodeJson

instance allMimeRenderJson :: EncodeJson a => AllMimeRender a JSON String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
