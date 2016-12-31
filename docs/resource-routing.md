# Resource Routing

`Hyper.Routing.ResourceRouter` aims to provide type-safe routing,
based on REST resources. It should not be possible to link, using an
HTML anchor, to a resource in the web application that does not exist,
or that does not handle the GET method. Neither should it be possible
to create a form that posts to a non-existing resource, or a resource
not handling POST requests.

## Resources

The central concept is *resources*, as in RESTful resources. Each
resource is a record describing its *path*, along with a set of HTTP
methods and handlers. Each method implemented must be specified
explicitly in the record with a `ResourceMethod` value, and those
values are parameterized with one of the marker types describing if it
is routed - `Supported` or `NotSupported`. The helper function
`handler` is used to construct `ResourceMethod` values with the
`Supported` type parameter.

```purescript
index =
  resource
  { path = []
  , "GET" = handler (html (h1 [] (text "Welcome!")))
  }
```

## Resource Routers

The `router` function creates a `ResourceRouter` out of a resource record. The
router tries to route HTTP requests to handlers in its resource. It should
also add the application resources as a type in the components of the Conn,
giving subsequent middleware access to that information. *The encoding of
resource types in the Conn is NOT supported yet.*

```purescript
app = runRouter defaultRouterFallbacks (router index)
```

The `ResourceRouter` provides an instance for `Alt`, making it possible to
chain resources and have them try to match the request in order.

```purescript
app =
  runRouter
  defaultRouterFallbacks
  (router index <|> router about <|> router contact)
```

### Router Fallbacks

The router has a _fallback_ concept - functions that provide a
response in case no resource matched the request. For instance, if a
route path matches the request URL, but not the method, the chain
short-circuits and the `onMethodNotAllowed` function provides a
response for the _405 Method Not Allowed_ case. The same goes for
`onNotFound`, in the case of a _404 Not Found_.

The `defaultRoutesFallbacks` can be used to get a set of basic
fallbacks. If custom responses are desired, simply provide your own
fallbacks, or override some of the defaults.

```purescript
fallbacks =
  { onNotFound:
    writeStatus statusNotFound
    >=> headers []
    >=> respond "What are you doing here?"
  , onMethodNotAllowed:
    \method ->
    writeStatus statusMethodNotAllowed
    >=> headers []
    >=> respond ("No way I'm routing a " <> show method <> " request.")
  }

app = runRouter fallbacks (router index)
```

## Type-Safe Links and Forms

The resource router module also provides functions that take resources as
arguments, creates links and forms to resources in the application *only if
they are in scope and support the required HTTP methods*. Paths are used from
the resource, so you cannot make a typo in the URL. In other words, mistakes in
routing and references between resources give you compile-time errors.

```purescript
about =
  resource
  { path = ["about"]
  , "GET" = handler (\conn -> respond
                              (linkTo contact (text "Contact Me!"))
                              conn)
  }

contact =
  resource
  { path = ["contact"]
  , "GET" = handler (respond (text "Good luck finding my email address."))
  }
```

As resources have to be in scope to be referred, you cannot refer to a
non-existing resource. You can, however, refer to an existing resource *that is
not routed*. This is described above in [Resource Routers](#resource-routers).

Erroneously using the `contact` resource together with `formTo` results in a
compile error, as there is no handler for the `POST` method in `contact`.

```text
Error found:
in module Example

  Could not match type

    Unsupported

  with type

    Supported

```
