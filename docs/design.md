# Design

## Connection

A connection models the entirety of a connection between the HTTP server and
the user agent - both request and response. This design is adopted from _Plug_,
an abstract HTTP interface in Elixir, that enables various HTTP libraries to
inter-operate.
