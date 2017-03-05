Type-Level Routing
==================

The `purescript-hyper-routing
<https://github.com/owickstrom/purescript-hyper-routing>`__ package provides
a concept called `routing types`. Its API, inspired heavily by the Haskell
library `Servant <https://haskell-servant.github.io>`__, lets us express web
application routing at the type-level.

By using routing types we get static guarantees about having handled all cases
and having correctly serializing and deserializing data. We also get a lot of
stuff for free, such as type-safe parameters for handlers, generated type-safe
URIs to endpoints, and generated clients and servers.

Based on the routing types, a couple of packages provide the machinery for
creating servers and clients using routing types:

.. toctree::
   :maxdepth: 1

   servers-for-routing-types
   automatically-derived-xhr-clients
