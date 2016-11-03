module Hyper.HTML where

import Prelude
import Data.Array (null)
import Data.Foldable (fold)
import Hyper.Response (toResponse, class Response)

type TagName = String

type AttrName = String
data Attr = Attr AttrName String

data Element
  = Element TagName (Array Attr) (Array Element)
  | Text String
    
-- TODO: Escape HTML
instance responseElement :: Response Element where
  toResponse element =
    case element of
      Element tagName attrs children ->
        let
          printAttr (Attr attrName value) = " " <> attrName <> "=" <> value
          attrsStr = if null attrs
                       then ""
                       else fold (map printAttr attrs)
          tagStartWithAttrs = "<" <> tagName <> ">"
        in
         tagStartWithAttrs
         <>
         fold (map toResponse children)
         <>
         "</" <> tagName <> ">"
      Text s -> s

a :: { href :: String } -> Array Element -> Element
a { href: h } = Element "a" [Attr "href" h]
