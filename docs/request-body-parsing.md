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
     (Aff (http :: HTTP, err :: EXCEPTION, avar :: AVAR | e))
     (Conn { body :: RequestBody
           , contentLength :: Maybe Int
           | req
           } res c)
     (Conn { body :: String
           , contentLength :: Maybe Int | req
           } res c)
```

A simple form parser can use `readBodyAsString` to convert the body a
more useful format for the application. The following function checks
the `Content-Type` header in the request, splits the request body,
builds up a `Form` value, and finally using that value for the `body`
field in the resulting `Conn`. The form body has type `Either Error
Form` to represent invalid forms.

``` purescript
parseForm ∷ forall m req res c.
            Applicative m =>
            Middleware
            m
            (Conn { body ∷ String
                  , headers :: StrMap String
                  | req
                  } res c)
            (Conn { body ∷ Either Error Form
                  , headers :: StrMap String
                  | req
                  }
                  res
                  c)
parseForm conn = ...
```

More efficient parsers, directly operating on the `RequestBody`,
instead of `String`, can of course be built as well.
