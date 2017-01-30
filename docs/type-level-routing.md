# Type-Level Routing

`Hyper.Routing.TypeLevelRouter` provides an API for expressing the web
application routes and their characterics as types, much like
[Servant][servant] does. From this type you get static guarantees about having
handled all cases, linking only to existing routes. You also get a lot of stuff
for free, such as type-safe parameters for handlers and links.

## A Single-Endpoint Example

Let's say we want to render a home page as HTML. We start out by declaring the
endpoint data type `Home`, and the structure of our site:

```purescript
data Home = Home

type MySite = Get HTML Home
```

`Get HTML Home` is a routing type with only one endpoint, rendering a `Home`
value as HTML. So where does the `Home` value come from? We provide it using a
*handler*. A handler for `MySite` would be some value of the following type:

```purescript
forall m. Monad m => ExceptT RoutingError m Home
```

We can construct such a value using `pure` and a `Home` value:

```purescript
home = pure Home
```

Nice! But what comes out on the other end? We need something that renders the
`Home` value as HTML. By providing an instance of `EncodeHTML` for `Home`, we
instruct the endpoint how to render.

```purescript
instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    p [] [ text "Welcome to my site!" ]
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
where no route matched the request, to the `router` function.

```purescript
onRoutingError status msg =
  writeStatus status
  >=> contentType textHTML
  >=> closeHeaders
  >=> respond (maybe "" id msg)

siteRouter = router mySite home onRoutingError
```

The value returned by `router` is regular middleware, ready to be passed to a
server.

```purescript
main =
  runServer defaultOptions onListening onRequestError {} siteRouter
  where
    onListening (Port port) =
      log ("Listening on http://localhost:" <> show port)

    onRequestError err =
      log ("Request failed: " <> show err)
```

## Routing Multiple Endpoints

Real-world servers often need more than one endpoint. Let's define a router for
an application that shows a home page with links, a page listing users, and a
page rendering a specific user.

```purescript
data Home = Home

data AllUsers = AllUsers (Array User)

newtype User = User { id :: Int, name :: String }

type MyOtherSite =
  Get HTML Home
  :<|> "users" :/ Get HTML AllUsers
  :<|> "users" :/ Capture "user-id" Int :> Get HTML User

otherSite :: Proxy MyOtherSite
otherSite = Proxy
```

Let's go through the new constructs used:

* `:<|>` is a type operator that separates *alternatives*. A router for this
  type will try each route in order until one matches.
* `:/` separates a literal path segment and the rest of the endpoint
  type.
* `Capture` takes a descriptive string and a type. It takes the next available
  path segment and tries to convert it to the given type. Each capture in an
  endpoint type corresponds to an argument in the handler function.
* `:>` separates a an endpoint modifier, like `Capture`, and the rest of the
  endpoint type.

We define handlers for our routes as regular functions on the specified data
types, returning `ExceptT RoutingError m a` values, where `m` is the monad of
our middleware, and `a` is the type to render for the endpoint.

```purescript
home :: forall m. Monad m => ExceptT RoutingError m Home
home = pure Home

allUsers :: forall m. Monad m => ExceptT RoutingError m AllUsers
allUsers = AllUsers <$> getUsers

getUser :: forall m. Monad m => Int -> ExceptT RoutingError m User
getUser id' =
  find userWithId <$> getUsers >>=
  case _ of
    Just user -> pure user
    Nothing ->
      throwError (HTTPError { status: statusNotFound
                            , message: Just "User not found."
                            })
  where
    userWithId (User u) = u.id == id'
```

As in the single-endpoint example, we want to render as HTML. Let's create
instances for our data types. Notice how we can create links between routes
in a type-safe manner.

```purescript
instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home =
    case linksTo otherSite of
      _ :<|> allUsers' :<|> _ ->
        p [] [ text "Welcome to my site! Go check out my "
             , linkTo allUsers' [ text "Users" ]
             , text "."
             ]

instance encodeHTMLAllUsers :: EncodeHTML AllUsers where
  encodeHTML (AllUsers users) =
    element_ "div" [ h1 [] [ text "Users" ]
                   , ul [] (map linkToUser users)
                   ]
    where
      linkToUser (User u) =
        case linksTo otherSite of
          _ :<|> _ :<|> getUser' ->
            li [] [ linkTo (getUser' u.id) [ text u.name ] ]

instance encodeHTMLUser :: EncodeHTML User where
  encodeHTML (User { name }) =
    h1 [] [ text name ]
```

The pattern match on the value returned by `linksTo` must match the structure
of the routing type. We use `:<|>` to pattern match on links. Each matched link
will have a type based on the corresponding endpoint. `getUser` in the
previous code has type `Int -> URI`, while `allUsers` has no captures and thus
has type `URI`.

We are still missing `getUsers`, our source of User values. In a real
application it would probably be a database query, but for this example we
simply hard-code some famous users of proper instruments.

```purescript
getUsers :: forall m. Applicative m => m (Array User)
getUsers =
  pure
  [ User { id: 1, name: "John Paul Jones" }
  , User { id: 2, name: "Tal Wilkenfeld" }
  , User { id: 3, name: "John Patitucci" }
  , User { id: 4, name: "Jaco Pastorious" }
  ]
```

Almost done! We just need to create the router, and start a server.

```purescript
main =
  let otherSiteRouter =
        router otherSite (home :<|> allUsers :<|> getUser) onRoutingError

      onRoutingError status msg =
        writeStatus status
        >=> contentType textHTML
        >=> closeHeaders
        >=> respond (maybe "" id msg)

      onListening (Port port) =
        log ("Listening on http://localhost:" <> show port)

      onRequestError err =
        log ("Request failed: " <> show err)

  in runServer defaultOptions
```

Notice how the composition of handler functions, using the value-level operator
`:<|>`, matches the structure of our routing type. If we fail to match the
type we get a compile error.

[servant]: https://haskell-servant.github.io
[proxy]: https://pursuit.purescript.org/packages/purescript-proxy/1.0.0/docs/Type.Proxy
