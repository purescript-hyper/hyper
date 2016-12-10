# Design

These are the central components of *Hyper*. While focusing heavily on
safety, Hyper tries to provide an open API that can support multiple
PureScript backends, and different styles of web applications.

## Conn

A *Conn*, short for "connection", models the entirety of a connection
between the HTTP server and the user agent - both request and
response. This design is adopted from _Plug_, an abstract HTTP
interface in Elixir, that enables various HTTP libraries to inter-operate.

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

The fields `state` and `writer` are usually present in the `response` record of
a Conn, as they are required by the functions in `Hyper.Response`. The state of
a response is tracked in type signatures to guarantee correctness in response
handling, preventing incorrect ordering of headers and body writes, incomplete
responses, or other such mistakes.

The `writer` is a value provided by the server of the application,
constrained by the `Hyper.Core.ResponseWriter` type class, and is used
by functions in `Hyper.Response` to provide higher-level ways of
responding to request. Let us have a look at the type signatures of
some of those functions.

We see that `headers` takes a traversable collection of headers, and gives
back a middleware that, given a connection *where headers are ready to be
written*, writes all specified headers, writes the separating CRLF before the
HTTP body, and *marks the state of the response as headers being closed*.

``` purescript
headers :: forall t m req res rw c.
           (Traversable t, Monad m, ResponseWriter rw m) =>
           t Header
        -> Middleware
           m
           (Conn req { writer :: rw, state :: HeadersOpen | res } c)
           (Conn req { writer :: rw, state :: HeadersClosed | res } c)
```

To be used in combination with `headers`, the `respond` function takes
some `Response r`, and gives back a middleware that, given a
connection *where all headers have been written*, writes a response,
and *marks the state of the response as ended*.

``` purescript
respond :: forall r m req res rw c.
           (Monad m, Response r, ResponseWriter rw m) =>
           r
        -> Middleware
           m
           (Conn req { writer :: rw, state :: HeadersClosed | res } c)
           (Conn req { writer :: rw, state :: ResponseEnded | res } c)
```

The `Response` type class describes types that can be written as responses.

``` purescript
class Response r where
  toResponse :: r -> String
```

**NOTE:** The return type of `toResponse` should probably be something
other than `String` ([GitHub issue](https://github.com/owickstrom/hyper/issues/5)).
