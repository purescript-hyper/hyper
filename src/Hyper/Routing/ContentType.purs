module Hyper.Routing.ContentType where

import Prelude
import Data.Argonaut (Json)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.List.NonEmpty (NonEmptyList)
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Data.Tuple (Tuple(..))
import Hyper.HTML (class EncodeHTML, HTML, encodeHTML)
import Hyper.HTML (asString) as HTML
import Hyper.Routing (type (:<|>))
import Type.Proxy (Proxy(..))

class HasMediaType ct where
  getMediaType :: Proxy ct -> MediaType

class MimeRender a ct b | a -> b, ct -> b  where
  mimeRender :: Proxy ct -> a -> b

class AllMimeRender a cts b | a -> b, cts -> b where
  allMimeRender :: Proxy cts -> a -> NonEmptyList (Tuple MediaType b)

instance allMimeRenderAltAlt :: ( MimeRender a ct1 b
                                , HasMediaType ct1
                                , AllMimeRender a (ct2 :<|> ct3) b
                                , HasMediaType ct2
                                )
                                => AllMimeRender a (ct1 :<|> ct2 :<|> ct3) b where
  allMimeRender _ a =
    pure (Tuple (getMediaType p) (mimeRender p a)) <> allMimeRender p' a
    where
      p = Proxy :: Proxy ct1
      p' = Proxy :: Proxy (ct2 :<|> ct3)

instance allMimeRenderCons :: ( MimeRender a ct1 b
                              , HasMediaType ct1
                              , MimeRender a ct2 b
                              , HasMediaType ct2
                              )
                              => AllMimeRender a (ct1 :<|> ct2) b where
  allMimeRender _ a =
    pure (Tuple (getMediaType p1) (mimeRender p1 a))
    <> pure (Tuple (getMediaType p2) (mimeRender p2 a))
    where
      p1 = Proxy :: Proxy ct1
      p2 = Proxy :: Proxy ct2


-- Instances for specific types


instance hasMediaTypeJson :: HasMediaType Json where
  getMediaType _ = applicationJSON

instance mimeRenderJson :: EncodeJson a => MimeRender a Json String where
  mimeRender _ = stringify <<< encodeJson

instance allMimeRenderJson :: EncodeJson a => AllMimeRender a Json String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))

instance hasMediaTypeHTML :: HasMediaType HTML where
  getMediaType _ = textHTML

instance mimeRenderHTML :: MimeRender HTML HTML String where
  mimeRender p = HTML.asString

instance mimeRenderHTMLEncodeHTML :: EncodeHTML a => MimeRender a HTML String where
  mimeRender _ = HTML.asString <<< encodeHTML

instance allMimeRenderHTML :: EncodeHTML a => AllMimeRender a HTML String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
