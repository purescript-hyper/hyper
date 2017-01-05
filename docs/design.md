# Design

We will start by looking at the central components of *Hyper*. While focusing
heavily on safety, Hyper tries to provide an open API that can support multiple
PureScript backends, and different styles of web applications.

The design of Hyper is inspired by a number of projects. The middleware chain
lends much from _Plug_, an abstract HTTP interface in Elixir, that enables
various HTTP libraries to inter-operate. You might also find similarities with
_connect_ in NodeJS. On the type system side, Hyper tries to bring in ideas
from _Haskell_ and _Idris_, specifically the use of phantom types and GADTs to
lift invariants to the type level and increase safety.

## Conn

A *Conn*, short for "connection", models the entirety of a connection
between the HTTP server and the user agent, both request and
response.

``` purescript
type Conn req res components = { request :: req
                               , response :: res
                               , components :: components
                               }
```

## Middleware

A *middleware* is a function transforming a `Conn` to another `Conn`,
in some monadic type `m`. The `Middleware` type synonym encapsulates
this concept, but it is still a regular function.

``` purescript
type Middleware m c c' = c -> m c'
```

Being able to parameterize `Conn` with some type `m`, you can customize the
chain depending on the needs of your middleware and handlers. Applications
can use monad transformers to track state, provide configuration, gather
metrics, and much more, in the chain of middleware.

## Response State Transitions

The `writer` field in the `response` record of a Conn is a value provided by
the server backend. Functions usually constrain the `writer` field to be a
value implementing the `ResponseWriter` type class. This makes it possible to
provide response writing abstractions without depending on a specific server
backend.

The state of a response writer is tracked in its type parameter. This
state tracking, and the type signatures of functions using the
response writer, guarantee correctness in response handling,
preventing incorrect ordering of headers and body writes, incomplete
responses, or other such mistakes. Let us have a look at the type
signatures of some of response writing functions in `Hyper.Response`.

We see that `headers` takes a traversable collection of headers, and gives
back a middleware that, given a connection *where headers are ready to be
written*, writes all specified headers, writes the separating CRLF before the
HTTP body, and *marks the state of the response writer as headers being closed*.

``` purescript
headers :: forall t m req res rw c.
           (Traversable t, Monad m, ResponseWriter rw m b) =>
           t Header
        -> Middleware
           m
           (Conn req { writer :: rw HeadersOpen | res } c)
           (Conn req { writer :: rw BodyOpen | res } c)
```

To be used in combination with `headers`, the `respond` function takes
some `Response m r b`, and gives back a middleware that, given a
connection *where all headers have been written*, writes a response,
and *marks the state of the response writer as ended*.

``` purescript
respond :: forall m r b req res rw c.
           (Monad m, Response m r b, ResponseWriter rw m b) =>
           r
        -> Middleware
           m
           (Conn req { writer :: rw BodyOpen | res } c)
           (Conn req { writer :: rw ResponseEnded | res } c)
```

The `Response` type class describes types that can be written as responses.  It
takes three type parameters, where `r` is the original response type, `m` is
usually an Applicative or a Monad in which the transformation can be performed,
and `b` is the target type.

``` purescript
class Response m r b | r -> b where
  toResponse :: r -> m b
```

This mechanism allows servers to provide specific types for the response body,
along with instances for common response types. When using the Node server,
which has a response body type wrapping `Buffer`, you can still respond with
a `String` or `HTML` value directly.

Aside from convenience in having a single function for most response types and
servers, the polymorphism of `respond` lets middleware be decoupled from
specific servers. It only requires an instance matching the response type used
by the middleware and the type required by the server.

