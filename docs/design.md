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
type Middleware e req req' res res' =
  Conn req res -> Aff (http :: HTTP | e) (Conn req' res')
```
