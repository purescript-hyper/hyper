# Use Cases

*Here follows a collection of loosely organized thoughts on how
to implement safe middleware in Hyper. Very much work-in-progress.*

## Parsing the Request Body

*Warning! Rough edges here, see
the [GitHub issue](https://github.com/owickstrom/hyper/issues/6) for
details.*

The request body is, when using the Node server, initially a `Readable
() e` in the connection. The user explicitly chooses to read and parse
the body with a given parser, which returns a new connection of a type
reflecting the action. The following type signature resides in
`Hyper.Node.Server`, and shows how a request body of a `Readable`
stream type can be read into a `String`.

```purescript
readBodyAsString ∷ ∀ m e req res c.
                   MonadEff (http ∷ HTTP, err ∷ EXCEPTION | e) m ⇒
                   Middleware
                   m
                   (Conn {body ∷ Readable () (http :: HTTP, err :: EXCEPTION | e) | req} res c)
                   (Conn {body ∷ Maybe String | req} res c)
```

A simple form parser can use `readBodyAsString` to convert the body a
more useful format for the application. The following function checks
the `content-type` header in the request, splits the request body,
builds up a `Form` value, and finally using that value for the `body`
field in the resulting `Conn`.

``` purescript
parseForm ∷ forall m req headers res c.
            Applicative m =>
            Middleware
            m
            (Conn { body ∷ String
                  , headers :: { "content-type" :: String
                               | headers
                               }
                  | req
                  } res c)
            (Either Error (Conn { body ∷ Form
                                , headers :: { "content-type" :: String
                                             | headers
                                             }
                                | req
                                }
                                res
                                c))
parseForm conn =
  case parseContentMediaType conn.request.headers."content-type" of
    Nothing -> pure (Left (error "Could not parse content-type header."))
    Just mediaType | mediaType == applicationFormURLEncoded -> pure do
      form <- splitPairs conn.request.body
      pure (conn { request = (conn.request { body = form }) })
    Just mediaType -> pure (Left (error $ "Invalid content media type: " <> show mediaType))
  where
    toTuple :: Array String -> Either Error (Tuple String String)
    toTuple kv =
      case kv of
        [key, value] → Right (Tuple (decodeURIComponent key) (decodeURIComponent value))
        parts        → Left (error ("Invalid form key-value pair: " <> joinWith " " parts))
    splitPair = split (Pattern "=")
    splitPairs ∷ String → Either Error Form
    splitPairs = split (Pattern "&")
                 >>> map splitPair
                 >>> map toTuple
                 >>> sequence
                 >>> map Form
```

More efficient parsers, directly operating on the `Readable` request body,
instead of `String`, can be built as well.

## Cohesion of Links, Forms, and Routes

It should not be possible to link, using an HTML anchor, to a resource in the
web application that does not exist, or that does not handle the GET method.
Neither should it be possible to create a form that posts to a non-existing
resource, or a resource not handling POST requests.

### Resources

Hyper has a concept of *resources*. Each resource is a record describing its
*path*, along with a set of HTTP methods and handlers. Each method implemented
in Hyper must be specified explicitly in the record with a `ResourceMethod`
value, and those values are parameterized with one of the marker types
describing if it is routed - `Supported` or `NotSupported`. The helper
functions `handler` and `notSupported` are used to construct `ResourceMethod`
values.

```purescript
index =
  { path: []
  , "GET": handler (html (h1 (text "Welcome!")))
  , "POST": notSupported
  }
```

### Resource Routers

The `resource` function creates a `ResourceRouter` that tries to route HTTP
requests to handlers in its resource. It should also add the application
resources as a type in the components of the Conn, giving subsequent middleware
access to that information. *The encoding of resource types in the Conn is NOT
supported yet.*

```purescript
app = fallbackTo notFound (resource index)
```

The `ResourceRouter` provides an instance for `Alt`, making it possible to
chain resources and have them try to match the request in order.

```purescript
app = fallbackTo notFound (resource index <|> resource about <|> resource contact)
```

### HTML DSL

A separate DSL for writing HTML, providing functions that take resources as
arguments, creates links and forms to resources in the application *only if
they are in scope and support the required HTTP methods*. Paths are used from
the resource, so you cannot make a typo in the URL. In other words, mistakes in
routing and references between resources give you compile-time errors.

```purescript
about =
  { path: ["about"]
  , "GET": handler (\conn -> html
                            (linkTo contact (text "Contact Me!"))
                            conn)
  , "POST": notSupported
  }

contact =
  { path: ["contact"]
  , "GET": handler (html (text "Good luck finding my email address."))
  , "POST": notSupported
  }
```

As resources have to be in scope to be referred, you cannot refer to a
non-existing resource. You can, however, refer to an existing resource *that is
not routed*. This is described above in [Resource Routers](#resource-routers).

Erroneously using the `contact` resource together with `formTo` results in a
compile error, as there is no handler for the `POST` method in `contact`.

```text
Error found:
in module Hyper.HTML.Example

  Could not match type

    Unsupported

  with type

    Supported

```
