********
Core API
********

This chapter explains the central components of Hyper, called the *Core API*.
While focusing heavily on safety, Hyper tries to provide an open API that can
support multiple PureScript backends, and different styles of web applications.

The design of Hyper is inspired by a number of projects. The middleware chain
lends much from *Plug*, an abstract HTTP interface in Elixir, that enables
various HTTP libraries to inter-operate. You might also find similarities with
*connect* in NodeJS. On the type system side, Hyper tries to bring in ideas
from *Haskell* and *Idris*, specifically the use of phantom types and GADTs to
lift invariants to the type level and increase safety.

The central components of the Core API are:

.. toctree::

  conn
  middleware
  response-state-transitions
  servers
