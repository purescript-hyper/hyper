module Hyper.HTML.DSL where

import Prelude
import Control.Monad.State (execState, modify, State)
import Data.Foldable (fold)
import Hyper.Conn (ResponseMiddleware)
import Hyper.HTML (Attr(Attr), Element(Text, Element))
import Hyper.Response (respond, toResponse)
import Hyper.Router (Supported, ResourceMethod, Path, pathToHtml)

type HTML r a = (State (Array Element) a)

execHTML :: forall r a. HTML r a -> Array Element
execHTML s = execState s []

addElement :: forall r. Element -> HTML r Unit
addElement e = modify ((<>) [e])

text :: forall r. String -> HTML r Unit
text = addElement <<< Text

withNested :: forall r.
              (Array Element -> Element)
           -> HTML r Unit
           -> HTML r Unit
withNested el = addElement <<< el <<< execHTML


h1 :: forall r.
      HTML r Unit
   -> HTML r Unit
h1 = withNested (Element "h1" [])

linkTo :: forall e req req' res res' c c' ms r.
          { path :: Path, "GET" :: ResourceMethod Supported e req req' res res' c c' | ms }
          -> HTML r Unit
          -> HTML r Unit
linkTo resource nested = do
  let children = execHTML nested
  addElement (Element "a" [Attr "href" (pathToHtml resource.path)] children)

formTo :: forall e req req' res res' c c' ms r.
          { path :: Path, "POST" :: ResourceMethod Supported e req req' res res' c c' | ms }
          -> HTML r Unit
          -> HTML r Unit
formTo resource nested = do
  let children = execHTML nested
  addElement (Element
              "form"
              [ Attr "method" "post"
              , Attr "action" (pathToHtml resource.path)
              ] children)

html :: forall r e res c. HTML r Unit
     -> ResponseMiddleware e { | res } { body :: String | res } c
html = respond <<< fold <<< map toResponse <<< execHTML
