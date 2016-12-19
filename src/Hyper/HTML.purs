module Hyper.HTML where

import Prelude
import Data.Array (null)
import Data.Foldable (fold)
import Data.Tuple (Tuple(Tuple))
import Hyper.Response (toResponse, class Response)

type TagName = String

type AttrName = String

type Attr = Tuple AttrName String

data Element
  = Element TagName (Array Attr) (Array Element)
  | Text String

-- TODO: Escape HTML
instance responseElement :: Response Element where
  toResponse element =
    case element of
      Element tagName attrs children ->
        let
          printAttr (Tuple attrName value) = " " <> attrName <> "=\"" <> value <> "\""
          attrsStr = if null attrs
                       then ""
                       else fold (map printAttr attrs)
          startTag = "<" <> tagName <> attrsStr <> ">"
          endTag = "</" <> tagName <> ">"
        in
         startTag
         <>
         fold (map toResponse children)
         <>
         endTag
      Text s -> s
