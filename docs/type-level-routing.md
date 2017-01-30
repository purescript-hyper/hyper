# Type-Level Routing

`Hyper.Routing.TypeLevelRouter` provides an API for expressing the web
application routes and their characterics as types, much like
[Servant][servant] does. From this type you get static guarantees about having
handled all cases, linking only to existing routes. You also get a lot of stuff
for free, such as type-safe parameters for handlers and links.

Let's say we want to render a `User` value as HTML on the `/` path. We start
out by declaring the structure of our site:

```purescript
newtype User = User { firstName :: String
                    , lastName :: String
                    }

type MySite = Get HTML User
```

`Get HTML User` describes a structure with only one endpoint, rendering a User
as HTML.

So where does the User value come from? We provide it using a *handler*. A
handler for `MySite` would be some value of the following type:

```purescript
forall m. Monad m => ExceptT RoutingError m User
```

We can construct such a value using `pure` and a `User` value:

```purescript
root = pure (User { firstName: "John", lastName: "Bonham" })
```

Nice! But what comes out on the other end? We need something that renders the
`User` value as HTML. The `MimeRender` type class encapsulates this concept.
We provide an instance for `User` and the `HTML` content type.

```purescript
instance mimeRenderUserHTML :: MimeRender User HTML String where
  mimeRender _ (User { firstName, lastName }) =
    asString $
    p [] [ text firstName
         , text " "
         , text lastName
         ]
```

We are getting ready to create the server. First, we need a value-level
representation of the `MySite` type, to be able to pass it to the `router`
function. For that we use [Proxy][proxy]. Its documentation describes it as
follows:

> The Proxy type and values are for situations where type information is
> required for an input to determine the type of an output, but where it is not
> possible or convenient to provide a value for the input.

We create a top-level definition of the type `Proxy MySite` with the value
constructor `Proxy`.

```purescript
mySite :: Proxy MySite
mySite = Proxy
```

We pass the proxy, our handler, and the `onRoutingError` function for cases
where no route matched the request.

```purescript
onRoutingError status msg =
  writeStatus status
  >=> contentType textHTML
  >=> closeHeaders
  >=> respond (maybe "" id msg)

siteRouter = router mySite root onRoutingError
```

The value returned by `router` is regular middleware, ready to be passed to a
server.

```purescript
main =
  runServer defaultOptions onListening onRequestError {} siteRouter
  where
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)
```

[servant]: https://haskell-servant.github.io
[proxy]: https://pursuit.purescript.org/packages/purescript-proxy/1.0.0/docs/Type.Proxy
