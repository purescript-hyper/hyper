module Hyper.Routing.Method
       ( Options
       , Get
       , Head
       , Post
       , Put
       , Patch
       , Delete
       , Trace
       , Connect
       ) where

import Hyper.Routing (Handler)

type Options = Handler "OPTIONS"

type Get = Handler "GET"

type Head = Handler "HEAD"

type Post = Handler "POST"

type Put = Handler "PUT"

type Patch = Handler "PATCH"

type Delete = Handler "DELETE"

type Trace = Handler "TRACE"

type Connect = Handler "CONNECT"
