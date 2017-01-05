# Servers

Although Hyper middleware are regular functions, which can applied to Conn
values, you often want a *server* to run your middleware. Hyper tries to be as
open as possible when it comes to servers -- your application, and the
middleware it depends on, should not be tied to a specific server. This allows
for greater reuse and the ability to test entire applications without running
the "real" server.

## NodeJS

The server in `Hyper.Node.Server` wraps the `http` module in NodeJS, and serves
middleware using the `Aff` monad. Here is how you can start a Node server:

``` purescript
let
  onListening (Port port) =
    log ("Listening on http://localhost:" <> show port)
  onRequestError err =
    log ("Request failed: " <> show err)
  app =
    writeStatus (Tuple 200 "OK")
    >=> closeHeaders
    >=> respond "Hello there!"
in runServer defaultOptions onListening onRequestError {} app
```

As seen above, `runServer` takes a record of options, two callbacks, an
initial *components* record, and your application middleware.

## Testing

When running tests you might not want to start a full HTTP server and sends
requests using an HTTP client. Instead you can use the server in
`Hyper.Test.TestServer`. It runs your middleware directly on `Conn` values, and
collects the response using a Writer monad. You get back a `TestResponse` from
which you can extract the status code, headers, and the response body.

``` purescript
it "responds with a friendly message" do
  conn <- { request: {}
          , response: { writer: testResponseWriter }
          , components: {}
          }
          # app
          # testServer
  testStatus conn `shouldEqual` Just statusOK
  testStringBody conn `shouldEqual` "Hello there!"
```
