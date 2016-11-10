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
web application that does not exist. Neither should it be possible to create
a form that posts to a non-existing resource. The *Router* module of Hyper
should encode the application route information as a component on the Conn.
A separate DSL for writing HTML can, based on the links and forms used in the
markup, create a type describing which resources must be present in the routing
component. A mismatch between those types would give a compile error, as the
user has referenced a non-existing route in the web application.

The application routes are described as a basic data type.

```purescript
data MyRoutes
  = GetGreeting
  | SaveGreeting
```

The user then needs to inform the router how to translate from and to `Route`
values, which are pairs of `Method` and `Path`. This can hopefully be done
automatically with `Generic` and deriving.

```purescript
instance routableMyRoutes :: Routable MyRoutes where
  fromPath url =
    case url of
      Route GET [] -> GetGreeting
      Route POST [] -> SaveGreeting
  toPath routes =
    case routes of
      GetGreeting -> Route GET []
      SaveGreeting -> Route POST []
```

Using this instance the `router` middleware can, given a function that maps from
`MyRoutes` to middleware, route requests. The type `MyRoutes` becomes part of
components of the Conn, and anchor and form tags can refer to that component to
ensure references are valid and type-checks.

### Open Issues

* HTTP method encoding in types
* Path parameters
* How to handle external links
