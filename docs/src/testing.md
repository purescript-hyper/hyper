# Testing

When running tests you might not want to start a full HTTP server and send
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
          # evalMiddleware app
          # testServer
  testStatus conn `shouldEqual` Just statusOK
  testStringBody conn `shouldEqual` "Hello there!"
```
