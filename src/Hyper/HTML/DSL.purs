module Hyper.HTML.DSL where

import Prelude
import Control.Monad.State (execState, modify, State)
import Control.Monad.State.Class (state, class MonadState)
import Data.Foldable (fold)
import Hyper.Conn (ResponseMiddleware)
import Hyper.HTML (Attr(Attr), Element(Text, Element))
import Hyper.Method (Method(POST, GET))
import Hyper.Response (StringResponse(StringResponse), respond, toResponse, class Response)
import Hyper.Router (toPath, Route(Route), class Routable)

type HTML r a = (State (Array Element) a)

execHTML :: forall r a. HTML r a -> Array Element
execHTML s = execState s []

addElement :: forall r. Element -> HTML r Unit
addElement e = modify ((<>) [e])

text :: forall r. String -> HTML r Unit
text = addElement <<< Text

withNested :: forall r.
              Routable r =>
              (Array Element -> Element)
           -> HTML r Unit
           -> HTML r Unit
withNested el = addElement <<< el <<< execHTML

{-
h1 :: forall r. Routable r =>
      HTML r Unit
   -> HTML r Unit
h1 = withNested (Element "h1" [])
-}

linkTo :: forall r.
          Routable r =>
          r
          -> HTML r Unit
          -> HTML r Unit
linkTo route nested = do
  let children = execHTML nested
  case toPath route of
    Route GET path -> addElement (Element "a" [Attr "href" path] children)
    Route POST _ -> pure unit -- TODO: need to split GET/POST in types somehow
                              -- so this cannot happen

{-

html do
  h1 "Welcome!"
  linkTo GetGreeting "Get a greeting!"
-}


html :: forall r e res c. HTML r Unit
     -> ResponseMiddleware e { | res } { body :: String | res } { routes :: r | c }
html = respond <<< StringResponse <<< fold <<< map toResponse <<< execHTML

