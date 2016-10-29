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
the `FormParser`.

``` purescript
data FormParser = FormParser

instance bodyParserFormParser :: BodyParser FormParser Form where
  parse _ = parseBodyFromString splitEntries
    where
      toTuple :: Array String -> Tuple String String
      toTuple [key, value] = Tuple (decodeURIComponent key)
                                   (decodeURIComponent value)
      toTuple _ = Tuple "omg" "no" -- TODO: Implement error handling
                                   --       in body parsers
      splitEntry = split (Pattern "=")
      splitEntries = Form <<<
                     map toTuple <<<
                     map splitEntry <<<
                     split (Pattern "&")

formParser :: forall e req h.
              RequestMiddleware
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
              , body :: Form
              | req
              }
formParser = parse FormParser
```

It uses the helper `parseBodyFromString` to parse the body as a
`www-form-urlencoded` form. Any invalid body will throw an error
in the Aff monad, which can be caught and handled.

## Enforcing Error Handling

*TODO!*
