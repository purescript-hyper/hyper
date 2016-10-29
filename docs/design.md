# Design

These are the central components of *Hyper*.

## Conn

A *Conn*, short for "connection", models the entirety of a connection
between the HTTP server and the user agent - both request and
response. This design is adopted from _Plug_, an abstract HTTP
interface in Elixir, that enables various HTTP libraries to inter-operate.

## Middleware

A *middleware* is a function transforming a `Conn` to another
`Conn`. The `Aff`  monad encapsulates asynchronicity and error
handling, as well as communicating the `HTTP` effect of applying middleware.

``` purescript
-- | The basic middleware type for transforming possibly both request and
-- | response.
type Middleware e req req' res res' =
  Conn req res -> Aff (http :: HTTP | e) (Conn req' res')
```

Many middleware transform either the request or the response. It is less common
that a single middleware transform both. Thus, Hyper provides two less general
type synonyms.

``` purescript
-- | A middleware that only transforms the request.
type RequestMiddleware e req req' =
  forall res. Middleware e req req' res res

-- | A middleware that only transforms the response.
type ResponseMiddleware e res res' =
  forall req. Middleware e req req res res'
```
