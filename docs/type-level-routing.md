# Type-Level Routing

`Hyper.Routing` provides an API for expressing web application routing as
types, much like [Servant][servant] does. By using routing types you get static
guarantees about having handled all cases. You also get a lot of stuff for
free, such as type-safe parameters for handlers, and automatically generated
type-safe URIs to endpoints.

_Future work might include automatic API documentation and sitemaps, automatic
deriving of PureScript AJAX clients, and more._

## A Single-Endpoint Example

Let's say we want to render a home page as HTML. We start out by declaring the
endpoint data type `Home`, and the structure of our site:

```purescript
data Home = Home

type Site1 = Get HTML Home
```

`Get HTML Home` is a routing type with only one endpoint, rendering a `Home`
value as HTML. So where does the `Home` value come from? We provide it using a
*handler*. A handler for `Site1` would be some value of the following type:

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
representation of the `Site1` type, to be able to pass it to the `router`
function. For that we use [Proxy][proxy]. Its documentation describes it as
follows:

> The Proxy type and values are for situations where type information is
> required for an input to determine the type of an output, but where it is not
> possible or convenient to provide a value for the input.

We create a top-level definition of the type `Proxy Site1` with the value
constructor `Proxy`.

```purescript
site1 :: Proxy Site1
site1 = Proxy
```

We pass the proxy, our handler, and the `onRoutingError` function for cases
where no route matched the request, to the `router` function.

```purescript
onRoutingError status msg =
  writeStatus status
  >=> contentType textHTML
  >=> closeHeaders
  >=> respond (maybe "" id msg)

site1Router = router site1 home onRoutingError
```

The value returned by `router` is regular middleware, ready to be passed to a
server.

```purescript
main =
  runServer defaultOptions onListening onRequestError {} site1Router
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

type Site2 =
  Get HTML Home
  :<|> "users" :/ Get HTML AllUsers
  :<|> "users" :/ Capture "user-id" Int :> Get HTML User

site2 :: Proxy Site2
site2 = Proxy
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
    case linksTo site2 of
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
        case linksTo site2 of
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
  let site2Router =
        router site2 (home :<|> allUsers :<|> getUser) onRoutingError

      onRoutingError status msg =
        writeStatus status
        >=> contentType textHTML
        >=> closeHeaders
        >=> respond (maybe "" id msg)

      onListening (Port port) =
        log ("Listening on http://localhost:" <> show port)

      onRequestError err =
        log ("Request failed: " <> show err)

  in runServer defaultOptions onListening onRequestError {} site2Router
```

Notice how the composition of handler functions, using the value-level operator
`:<|>`, matches the structure of our routing type. If we fail to match the
type we get a compile error.

## Content Negotiation

By specifying alternative content types for an endpoint, Hyper can choose a
response and content type based on the request `Accept` header. This is called
_content negotiation_. Instead of specifying a single type, like `HTML` or
`Json`, we provide alternatives using `:<|>`. All content types must have
`MimeRender` instances for the response body type.

```purescript
type Site3 =
  Get (HTML :<|> HTML) Home
  :<|> "users" :/ Get (HTML :<|> Json) AllUsers
  :<|> "users" :/ Capture "user-id" Int :> Get (HTML :<|> JSON) User
```

By making requests to this site, using `Accept` headers, we can see how the
router chooses the matching content type (output formatted and shortened for
readability).

```bash
$ curl -H 'Accept: application/json' http://localhost:3000/users
[
  {
    "name": "John Paul Jones",
    "id": "1"
  },
  {
    "name": "Tal Wilkenfeld",
    "id": "2"
  },
  ...
]
```

```bash
$ curl -H 'Accept: text/html' http://localhost:3000/users
<div>
  <h1>Users</h1>
  <ul>
    <li><a href="/users/1">John Paul Jones</a></li>
    <li><a href="/users/2">Tal Wilkenfeld</a></li>
    ...
  </ul>
</div>
```

[servant]: https://haskell-servant.github.io
[proxy]: https://pursuit.purescript.org/packages/purescript-proxy/1.0.0/docs/Type.Proxy
