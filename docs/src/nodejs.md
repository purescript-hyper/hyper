# NodeJS

The server in `Hyper.Node.Server` wraps the `http` module in NodeJS, and serves
middleware using the `Aff` monad. Here is how you can start a Node server:

``` purescript
let
  app =
    writeStatus (Tuple 200 "OK")
    :*> closeHeaders
    :*> respond "Hello there!"
in runServer defaultOptions {} app
```

As seen above, `runServer` takes a record of options, an initial
*components* record, and your application middleware. If you want to
do logging on server startup, and on any request handling errors, use
`defaultOptionsWithLogging`.

## Monad Transformers

You might want to use a monad transformer stack in your application,
for instance as a way to pass configuration, or to accumulate some
state in the chain of middleware. The underlying monad of `Middleware`
is parameterized for this exact purpose. When running the NodeJS
server with monad transformers, you need to pass a function in the
options record, in the `runM` field.

The following code runs a middleware using the `ReaderT` monad
transformer. Note that the `runAppM` function might need to be defined
at the top-level to please the type checker.

```{include=docs/src/nodejs/NodeReaderT.purs snippet=main .purescript}
```

In a real-world application the configuration type `MyConfig` could
hold a database connection pool, or settings read from the environment,
for example.
