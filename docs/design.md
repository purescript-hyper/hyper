# Design

These are the central components of *Hyper*.

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

A *middleware* is a function transforming a `Conn` to another
`Conn`, in some type `m` (possibly a monad stack). The `Middleware`
type synonym encapsulates this concept, but it is still a regular
function.

``` purescript
type Middleware m c c' = c -> m c'
```

Many middleware transform either the request or the response. It is less common
that a single middleware transform both. Thus, Hyper provides two less general
type synonyms.

``` purescript
-- | A middleware that only transforms the request.
type RequestMiddleware m req req' c =
  forall res. Middleware m (Conn req res c) (Conn req' res c)

-- | A middleware that only transforms the response.
type ResponseMiddleware m res res' c =
  forall req. Middleware m (Conn req res c) (Conn req res' c)
```
