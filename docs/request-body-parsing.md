# Request Body Parsing

The request body is, when using the Node server, initially a
`RequestBody` in the connection. The user explicitly chooses to read
and parse the body with a given parser, which returns a new connection
of a type reflecting the action. The following type signature resides
in `Hyper.Node.Server`, and shows how a request body can be read into
a `String`. The `Aff` monad, and the `AVAR` effect, is used to
accomplish this asynchronously in the case of the Node server.

```purescript
readBodyAsString
  :: forall e req res c.
     Middleware
     (Aff (http :: HTTP, err  :: EXCEPTION, avar  :: AVAR | e))
     (Conn { body :: RequestBody
           , contentLength :: Maybe Int
           | req
           } res c)
     (Conn {body :: String, contentLength :: Maybe Int | req} res c)
     Unit
```

A simple form parser can use `readBodyAsString` to convert the body a
more useful format for the application. As an example, the following function
checks the `Content-Type` header in the request, splits the request body, builds
up a `Form` value, and returns the value, with type `Either Error Form` to
represent possibly invalid forms.

``` purescript
parseForm :: forall m req res c.
            (IxMonadMiddleware m, IxMonad m) =>
            m
            (Conn { body :: String
                  , headers :: StrMap String
                  | req
                  } res c)
            (Conn { body :: String
                  , headers :: StrMap String
                  | req
                  }
                  res
                  c)
            (Either Error Form)
parseForm =
  getConn :>>= \conn ->
  case lookup "content-type" conn.request.headers >>= parseContentMediaType of
    Nothing ->
      ipure (throwError (error "Missing or invalid content-type header."))
    Just mediaType | mediaType == applicationFormURLEncoded ->
      ipure (Form <$> splitPairs conn.request.body)
    Just mediaType ->
      ipure (throwError (error ("Cannot parse media of type: " <> show mediaType)))
```

The `parseForm` function can then be used inside a middleware chain.

```purescript
parseForm :>>=
case _ of
  Left err -> ...
  Right (Form values) -> ...
```

More efficient parsers, directly operating on the `RequestBody`,
instead of `String`, can of course be built as well.
