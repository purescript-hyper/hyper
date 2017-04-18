Type-Level Routing
==================

The `Trout
<https://github.com/owickstrom/purescript-trout>`__ package provides
`type-level routing`. Its API, inspired heavily by the Haskell library `Servant
<https://haskell-servant.github.io>`__, lets us express web application routing
at the type-level using `routing types`.

By using routing types we get static guarantees about having handled all cases
and having correctly serializing and deserializing data. We also get a lot of
stuff for free, such as type-safe parameters for handlers, generated type-safe
URIs to endpoints, and generated clients and servers.

Packages
--------

The following packages are available for type-level routing with Hyper:

`Trout <https://github.com/owickstrom/purescript-trout>`__
  Provides the core types used in routing types. It does not depend on Hyper,
  and can be used for other libraries and frameworks, theoretically.
`Hypertrout <https://github.com/owickstrom/purescript-hypertrout>`__
  Used to create routers based on routing types, which are Hyper middleware. It
  can be seen as the equivalent of `servant-server`.
`Trout Client <https://github.com/owickstrom/purescript-trout-client>`__
  Derive client-side accessor functions for doing AJAX requests, based on Trout
  routing types. Use this together with `Hypertrout` to get an all-PureScript
  project, with safe routing between client and server.
