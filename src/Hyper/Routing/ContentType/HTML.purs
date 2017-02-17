module Hyper.Routing.ContentType.HTML
       ( HTML
       , linkTo
       , class EncodeHTML
       , encodeHTML
       ) where

import Prelude
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Data.URI (URI, printURI)
import Hyper.Routing.ContentType (class AllMimeRender, class HasMediaType, class MimeRender, getMediaType, mimeRender)
import Text.Smolder.HTML (a)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup (Markup, MarkupM, (!))
import Text.Smolder.Renderer.String (render)

data HTML

linkTo :: URI -> Markup Unit -> Markup Unit
linkTo uri = a ! href (printURI uri)

class EncodeHTML a where
  encodeHTML :: a -> Markup Unit

instance hasMediaTypeHTML :: HasMediaType HTML where
  getMediaType _ = textHTML

instance mimeRenderHTML :: MimeRender (MarkupM Unit Unit) HTML String where
  mimeRender p = render

instance mimeRenderHTMLEncodeHTML :: EncodeHTML a => MimeRender a HTML String where
  mimeRender _ = render <<< encodeHTML

instance allMimeRenderHTML :: EncodeHTML a => AllMimeRender a HTML String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
