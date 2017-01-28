module Hyper.Routing.TypeLevelRouter.Method
       ( Options
       , Get
       , Head
       , Post
       , Put
       , Delete
       , Trace
       , Connect
       ) where

import Hyper.Routing.TypeLevelRouter (Handler)

type Options = Handler "OPTIONS"

type Get = Handler "GET"

type Head = Handler "HEAD"

type Post = Handler "POST"

type Put = Handler "PUT"

type Delete = Handler "DELETE"

type Trace = Handler "TRACE"

type Connect = Handler "CONNECT"
