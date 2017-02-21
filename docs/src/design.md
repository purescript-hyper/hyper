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
type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }
```

The `request` and `response` hold the values representing the HTTP request and
response, respectively. The purpose of the `components` field, however, is not
that obvious. It is used for things not directly related to the HTTP, but
nonetheless related to the act of responding to the HTTP request. A middleware
can add information the Conn using components, like providing authentication or
authorization values. The types of these components then becomes part of the
Conn type, and you get compile-time guarantees when using the provided
components.

## Middleware

A *middleware* is an *indexed monadic action* transforming one `Conn` to another
`Conn`. It operates in some base monad `m`, and is indexed by `i` and `o`, the
*input* and *output* Conn types of the middleware action.

``` purescript
newtype Middleware m i o a = ...
```

The input and output type parameters are used to ensure that a Conn is
transformed, and that side-effects are performed, correctly, throughout the
middleware chain.

Being able to parameterize `Middleware` with some type `m`, you can customize
the chain depending on the needs of your middleware and handlers. Applications
can use monad transformers to track state, provide configuration, gather
metrics, and much more, in the chain of middleware.

Middleware are composed using `ibind`, the indexed monadic version of `bind`.
The simplest way of composing middleware is by chaining them with `:*>`, from
`Control.IxMonad`. See [purescript-indexed-monad][ixmonad] for more information.

``` purescript
writeStatus statusOK
:*> closeHeaders
:*> respond "We're composing middleware!"
```

If you want to feed the return value of one middleware into another, use `:>>=`,
the infix operator alias for `mbind`.

```purescript
getUser :>>= renderUser
```

You can also rebind the *do block* syntax to use `ibind` instead of regular
`bind`.

``` purescript
do
  user <- getUser
  writeStatus statusOK
  closeHeaders
  respond ("User: " <> user.name)
  where bind = ibind
```

## Response State Transitions

The `writer` field in the `response` record of a Conn is a value provided by
the server backend. Middleware often constrain the `writer` field to be a
value implementing the `ResponseWriter` type class. This makes it possible to
provide response writing abstractions without depending on a specific server
backend.

The state of a response writer is tracked in its type parameter. This
state tracking, and the type-indexed middleware using the response writer,
guarantee correctness in response handling, preventing incorrect ordering of
headers and body writes, incomplete responses, or other such mistakes. Let us
have a look at the type signatures of some of response writing functions in
`Hyper.Response`.

We see that `headers` takes a traversable collection of headers, and gives
back a middleware that, given a connection where headers are ready to be
written (`HeadersOpen`), writes all specified headers, writes the separating
CRLF before the HTTP body, and marks the state of the response writer as being
ready to write the body (`BodyOpen`).

``` purescript
headers :: forall t m req res rw c.
           (Traversable t, Monad m, ResponseWriter rw m b) =>
           t Header
        -> Middleware
           m
           (Conn req { writer :: rw HeadersOpen | res } c)
           (Conn req { writer :: rw BodyOpen | res } c)
           Unit
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
           Unit
```

The `Response` type class describes types that can be written as responses.  It
takes three type parameters, where `b` is the target type, `m` is a base monad
for the Middleware returned, and `r` is the original response type,

``` purescript
class Response b m r where
  toResponse :: forall i. r -> Middleware m i i b
```

This mechanism allows servers to provide specific types for the response body,
along with instances for common response types. When using the Node server,
which has a response body type wrapping `Buffer`, you can still respond with
a `String` or `HTML` value directly.

Aside from convenience in having a single function for most response types and
servers, the polymorphism of `respond` lets middleware be decoupled from
specific servers. It only requires an instance matching the response type used
by the middleware and the type required by the server.

[ixmonad]: https://pursuit.purescript.org/packages/purescript-indexed-monad/0.1.1
