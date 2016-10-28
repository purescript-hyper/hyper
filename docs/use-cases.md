# Use Cases

## Writing a Full Response

A function which writes a complete response could take a connection with an
empty response.

```purescript
class ResponseWriter w where
  writeResponse :: String
                -> w
                -> Conn { response :: Complete }

instance forall r. ResponseWriter (Conn { response :: Empty } | r) where
  writeResponse r conn = ...
```

## Writing a Partial Response

A function which writes a partial response could take a connection of either an
empty or a partial response.

```purescript
class PartialWriter w where
  writePartial :: String
               -> w
               -> Conn { response :: Partial }
```

## Parsing the Request Body

The request body is a stream in the connection. It might not always be
of interest, thus it is not read, and not parsed, by default. Instead,
the user explicitly chooses to read and parse the body with a given
parser, which returns a new connection of a type reflecting the action.

```purescript
class BodyParser t p where
  parse ∷ forall r. Conn { bodyStream ∷ Stream Unconsumed | r }
        -> Conn { bodyStream ∷ Stream Consumed, body ∷ t | r }

instance BodyParser Form HttpFormParamsParser where
  parse conn = ...
```

Given this type, the request body can neither be read more than once,
nor can the connection's `body` be overwritten.
