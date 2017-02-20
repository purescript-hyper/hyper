# Type-Level Routing

`Hyper.Routing` provides an API for expressing web application routing as
types, much like [Servant][servant] does. By using routing types you get static
guarantees about having handled all cases. You also get a lot of stuff for
free, such as type-safe parameters for handlers, and automatically generated
type-safe URIs to endpoints.

## A Single-Endpoint Example

Let's say we want to render a home page as HTML. We start out by declaring the
endpoint data type `Home`, and the structure of our site:


```{.purescript include=docs/type-level-routing-examples/Site1.purs snippet=routing-type}
```

`Get HTML Home` is a routing type with only one endpoint, rendering a `Home`
value as HTML. So where does the `Home` value come from? We provide it using a
*handler*. A handler for `Site1` would be some value of the following type:

```purescript
forall m. Monad m => ExceptT RoutingError m Home
```

We can construct such a value using `pure` and a `Home` value:

```{.purescript include=docs/type-level-routing-examples/Site1.purs snippet=handler}
```

Nice! But what comes out on the other end? We need something that renders the
`Home` value as HTML. By providing an instance of `EncodeHTML` for `Home`, we
instruct the endpoint how to render.

```{.purescript include=docs/type-level-routing-examples/Site1.purs snippet=encoding}
```

The `HTML` type is a phantom type, only used as a marker type, and the actual
markup is written in the `MarkupM` DSL from [purescript-smolder][smolder].

We are getting ready to create the server. First, we need a value-level
representation of the `Site1` type, to be able to pass it to the `router`
function. For that we use [Proxy][proxy]. Its documentation describes it as
follows:

> The Proxy type and values are for situations where type information is
> required for an input to determine the type of an output, but where it is not
> possible or convenient to provide a value for the input.

We create a top-level definition of the type `Proxy Site1` with the value
constructor `Proxy`.

```{.purescript include=docs/type-level-routing-examples/Site1.purs snippet=proxy}
```

We pass the proxy, our handler, and the `onRoutingError` function for cases
where no route matched the request, to the `router` function.

```{.purescript include=docs/type-level-routing-examples/Site1.purs snippet=router}
```

The value returned by `router` is regular middleware, ready to be passed to a
server.

```{.purescript include=docs/type-level-routing-examples/Site1.purs snippet=main}
```

## Routing Multiple Endpoints

Real-world servers often need more than one endpoint. Let's define a router for
an application that shows a home page with links, a page listing users, and a
page rendering a specific user.

```{.purescript include=docs/type-level-routing-examples/Site2.purs snippet=resources-and-type}
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

```{.purescript include=docs/type-level-routing-examples/Site2.purs snippet=handlers}
```

As in the single-endpoint example, we want to render as HTML. Let's create
instances for our data types. Notice how we can create links between routes
in a type-safe manner.

```{.purescript include=docs/type-level-routing-examples/Site2.purs snippet=encoding}
```

The pattern match on the value returned by `linksTo` must match the structure
of the routing type. We use `:<|>` to pattern match on links. Each matched link
will have a type based on the corresponding endpoint. `getUser` in the
previous code has type `Int -> URI`, while `allUsers` has no captures and thus
has type `URI`.

We are still missing `getUsers`, our source of User values. In a real
application it would probably be a database query, but for this example we
simply hard-code some famous users of proper instruments.

```{.purescript include=docs/type-level-routing-examples/Site2.purs snippet=get-users}
```

Almost done! We just need to create the router, and start a server.

```{.purescript include=docs/type-level-routing-examples/Site2.purs snippet=main}
```

Notice how the composition of handler functions, using the value-level operator
`:<|>`, matches the structure of our routing type. If we fail to match the
type we get a compile error.

## Content Negotiation

By specifying alternative content types for an endpoint, Hyper can choose a
response and content type based on the request `Accept` header. This is called
_content negotiation_. Instead of specifying a single type, like `HTML` or
`JSON`, we provide alternatives using `:<|>`. All content types must have
`MimeRender` instances for the response body type.

```{.purescript include=docs/type-level-routing-examples/Site3.purs snippet=routing-type}
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

There's experimental support for _wildcards_ and _qualities_ as well.

```bash
$ curl -H 'Accept: text/*;q=1.0' http://localhost:3000/users
<div>
  <h1>Users</h1>
  <ul>
    <li><a href="/users/1">John Paul Jones</a></li>
    <li><a href="/users/2">Tal Wilkenfeld</a></li>
    ...
  </ul>
</div>
```

## Automatically Derived XHR Clients

As we represent routing as types, we can derive XHR clients from those types.
In a client-side application, for example one written using [Pux][pux] or
[Halogen][halogen], you can use the [purescript-hyper-routing-xhr][routing-xhr]
library to derive such client functions. You get functions with type-safe
parameters and encoding/decoding, using your routed types, and results in the
[Aff][aff] monad, which are easily integrated into most frameworks.

[servant]: https://haskell-servant.github.io
[smolder]: https://github.com/mobil/purescript-smolder
[proxy]: https://pursuit.purescript.org/packages/purescript-proxy/1.0.0/docs/Type.Proxy
[routing-xhr]: https://github.com/owickstrom/purescript-hyper-routing-xhr
[pux]: https://www.purescript-pux.org
[halogen]: https://github.com/slamdata/purescript-halogen
[aff]: https://github.com/slamdata/purescript-aff
