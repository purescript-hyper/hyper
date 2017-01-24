module Hyper.HTML
       ( TagName
       , HTML
       , AttrName
       , Attr
       , asString
       , element
       , element_
       , text
       -- Utility functions.
       , h1
       , h2
       , h3
       , h4
       , h5
       , h6
       , p
       , ul
       , ol
       , li
       , span
       , div
       , a
       , form
       ) where

import Prelude
import Data.Array (null)
import Data.Foldable (fold)
import Data.Tuple (Tuple(Tuple))

type TagName = String

type AttrName = String

type Attr = Tuple AttrName String


data HTML
  = Element TagName (Array Attr) (Array HTML)
  | Text String


-- TODO: Escape HTML
asString :: HTML -> String
asString =
  case _ of
    Element tagName attrs children ->
      let
        printAttr (Tuple attrName value) = " " <> attrName <> "=\"" <> value <> "\""
        attrsStr = if null attrs
                   then ""
                   else fold (map printAttr attrs)
        startTag = "<" <> tagName <> attrsStr <> ">"
        endTag = "</" <> tagName <> ">"
      in startTag <> fold (map asString children) <> endTag
    Text s -> s

-- | Create an HTML text.
text :: String -> HTML
text = Text


-- | Create an element without any attributes.
element :: TagName -> Array Attr -> Array HTML -> HTML
element = Element


-- | Create an element without any attributes.
element_ :: TagName -> Array HTML -> HTML
element_ tagName = Element tagName []

--
-- Convinience functions for elements.
--

h1 :: Array Attr -> Array HTML -> HTML
h1 = Element "h1"

h2 :: Array Attr -> Array HTML -> HTML
h2 = Element "h2"

h3 :: Array Attr -> Array HTML -> HTML
h3 = Element "h3"

h4 :: Array Attr -> Array HTML -> HTML
h4 = Element "h4"

h5 :: Array Attr -> Array HTML -> HTML
h5 = Element "h5"

h6 :: Array Attr -> Array HTML -> HTML
h6 = Element "h6"

p :: Array Attr -> Array HTML -> HTML
p = Element "p"

ul :: Array Attr -> Array HTML -> HTML
ul = Element "ul"

ol :: Array Attr -> Array HTML -> HTML
ol = Element "ol"

li :: Array Attr -> Array HTML -> HTML
li = Element "li"

span :: Array Attr -> Array HTML -> HTML
span = Element "span"

div :: Array Attr -> Array HTML -> HTML
div = Element "div"

a :: Array Attr -> Array HTML -> HTML
a = Element "a"

form :: Array Attr -> Array HTML -> HTML
form = Element "form"
