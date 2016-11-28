# Use Cases

*Here follows a collection of still loosely organized thoughts on how
to implement safe middleware in Hyper. Very much work-in-progress.*

## Writing a Full Response

A function which writes a complete response could take a `Conn` with an
empty response.

```purescript
respond s :: forall e res h. String
          -> ResponseMiddleware
             e
             { headers :: {
                          | h
                          }
             , body :: Stream Write Initial
             | res
             }
             { headers :: { "content-type" :: String
                          , "content-length" :: String
                          | h
                          }
             , body :: Stream Write Closed
             | res
             }
```

## Parsing the Request Body

The request body is initially a `Stream Read Initial` in the connection. It
might not always be of interest, thus it is not read, and not parsed, by
default. Instead, the user explicitly chooses to read and parse the body with a
given parser, which returns a new connection of a type reflecting the action.

```purescript
class BodyParser p t | p -> t where
  parse :: forall e req h. p
        -> RequestMiddleware
           e
           { bodyStream :: Stream Read Initial
           , headers :: { "content-type" :: String
                        , "content-length" :: String
                        | h
                        }
           | req
           }
           { bodyStream :: Stream Read Closed
           , headers :: { "content-type" :: String
                        , "content-length" :: String
                        | h
                        }
           , body :: t
           | req
           }
```

Given this type, the request body can neither be read more than once,
nor can the connection's `body` be overwritten. An example parser is
the `BodyParser` instance for `FormParser` and `Form`.

``` purescript
-- | A form represents a "www-form-urlencoded" form.
newtype Form = Form (Array (Tuple String String))

-- | Placeholder constructor without any options.
data FormParser = FormParser

instance bodyParserFormParser :: BodyParser FormParser Form where
  parse _ = parseBodyFromString splitPairs
    where
      toTuple :: Array String -> Either Error (Tuple String String)
      toTuple kv =
        case kv of
          [key, value] → Right (Tuple (decodeURIComponent key)
                                      (decodeURIComponent value))
          parts        → Left (error ("Invalid form key-value pair: "
                                      <> joinWith " " parts))
      splitPair = split (Pattern "=")
      splitPairs ∷ String → Either Error Form
      splitPairs = (<$>) Form
                   <<< sequence
                   <<< map toTuple
                   <<< map splitPair
                   <<< split (Pattern "&")
```

This instance uses the helper `parseBodyFromString` to first read the body as a
string, then parse that string as a `www-form-urlencoded` form. Any invalid form
will throw an error in the Aff monad, which can be caught and handled.

## Enforcing Error Handling

*TODO!*

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
  , "GET": handler (\conn -> html
                             (linkTo about (text "About Me"))
                             conn)
  , "POST": notSupported
  }
```

As resources have to be in scope to be referred, you cannot refer to a
non-existing resource. You can, however, refer to an existing resource *that is
not routed*. This is described above in [Resource Routers](#resource-routers).

Erroneously using the `about` resource together with `formTo` results in a
compile error.

```text
Error found:
in module Hyper.HTML.Example

  Could not match type

    Unsupported

  with type

    Supported

```

### Open Issues

* Encoding routed resources in the Conn type (see [Resource
    Routers](#resource-routers))
* Path parameters (Hyper currently only supports literal path segments)
* How to handle external links
