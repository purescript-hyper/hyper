module Hyper.HTML.DSL where

import Prelude
import Control.Monad.State (execState, modify, State)
import Data.Foldable (fold)
import Hyper.Core (class ResponseWriter, ResponseEnded, HeadersClosed, Conn, Middleware)
import Hyper.HTML (Attr(Attr), Element(Text, Element))
import Hyper.Response (respond, toResponse)
import Hyper.Router (Supported, ResourceMethod, Path, pathToHtml)

type HTML a = (State (Array Element) a)

execHTML :: forall a. HTML a -> Array Element
execHTML s = execState s []

addElement :: Element -> HTML Unit
addElement e = modify ((<>) [e])

text :: String -> HTML Unit
text = addElement <<< Text

withNested :: (Array Element -> Element)
           -> HTML Unit
           -> HTML Unit
withNested el = addElement <<< el <<< execHTML


h1 :: HTML Unit
   -> HTML Unit
h1 = withNested (Element "h1" [])

linkTo :: forall m c c' ms.
          { path :: Path
          , "GET" :: ResourceMethod Supported m c c'
          | ms }
          -> HTML Unit
          -> HTML Unit
linkTo resource nested = do
  let children = execHTML nested
  addElement (Element "a" [Attr "href" (pathToHtml resource.path)] children)

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
              [ Attr "method" "post"
              , Attr "action" (pathToHtml resource.path)
              ] children)

html :: forall m req res rw c.
        (Monad m, ResponseWriter rw m) =>
        HTML Unit
     -> Middleware
        m
        (Conn req { writer :: rw HeadersClosed | res } c)
        (Conn req { writer :: rw ResponseEnded | res } c)
html = respond <<< fold <<< map toResponse <<< execHTML
