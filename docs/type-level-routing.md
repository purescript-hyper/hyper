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

_**TODO:** Show how to construct a router and runnable server. Also talk about
value-level respresentation using Proxy._

[servant]: https://haskell-servant.github.io
