module Hyper.HTML.DSL where

import Prelude
import Control.Monad.State (execState, modify, State)
import Data.Foldable (fold)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (class ResponseWriter, ResponseEnded, BodyOpen, Conn, Middleware)
import Hyper.HTML (TagName, Attr, Element(Text, Element))
import Hyper.Response (respond, toResponse)
import Hyper.Router (Supported, ResourceMethod, Path, pathToHtml)

type HTML a = (State (Array Element) a)

execHTML :: forall a. HTML a -> Array Element
execHTML s = execState s []

addElement :: Element -> HTML Unit
addElement e = modify (_ <> [e])

text :: String -> HTML Unit
text = addElement <<< Text

withNested :: (Array Element -> Element)
           -> HTML Unit
           -> HTML Unit
withNested el = addElement <<< el <<< execHTML

element :: TagName
        -> Array Attr
        -> HTML Unit
        -> HTML Unit
element tagName attrs = withNested (Element tagName attrs)

linkTo :: forall m c c' ms.
          { path :: Path
          , "GET" :: ResourceMethod Supported m c c'
          | ms }
          -> HTML Unit
          -> HTML Unit
linkTo resource nested = do
  let children = execHTML nested
  addElement (Element "a" [Tuple "href" (pathToHtml resource.path)] children)

formTo :: forall m c c' ms.
          { path :: Path
          , "POST" :: ResourceMethod Supported m c c'
          | ms
          }
          -> HTML Unit
          -> HTML Unit
formTo resource nested = do
  let children = execHTML nested
  addElement (Element
              "form"
              [ Tuple "method" "post"
              , Tuple "action" (pathToHtml resource.path)
              ] children)

html :: forall m req res rw c.
        (Monad m, ResponseWriter rw m) =>
        HTML Unit
     -> Middleware
        m
        (Conn req { writer :: rw BodyOpen | res } c)
        (Conn req { writer :: rw ResponseEnded | res } c)
html = respond <<< fold <<< map toResponse <<< execHTML

--
-- Convinience functions for elements.
--

type DSLElement = Array Attr -> HTML Unit -> HTML Unit

h1 :: DSLElement
h1 = element "h1"

h2 :: DSLElement
h2 = element "h2"

h3 :: DSLElement
h3 = element "h3"

h4 :: DSLElement
h4 = element "h4"

h5 :: DSLElement
h5 = element "h5"

h6 :: DSLElement
h6 = element "h6"

p :: DSLElement
p = element "p"

ul :: DSLElement
ul = element "ul"

ol :: DSLElement
ol = element "ol"

li :: DSLElement
li = element "li"

span :: DSLElement
span = element "span"

-- `<a>` is, for now, intentionally left out to encourage use of `linkTo`.
