# Changelog

* 0.3.0
  - Support usage of monad transformers together with Node server.
  - Move `onListening` and `onRequestError` into the Node server
    options.
  - Move the documentation to http://hyper.wickstrom.tech.
  - Support streaming responses with the Node server.
  - Restructure documentation and examples, more documentation.
  - Redesign of Middleware construct to use [indexed monads][indexed],
    instead of monadic functions. This breaks backwards compatibility!
  - Basic support for multiple content types in router (content negotiation
    is not at all well-tested, beware!)
* 0.2.0
  - Use `purescript-http-methods` instead of custom Method type. Also use
    `Either Method CustomMethod` in middleware.
* ... The dark ages.

[indexed]: https://github.com/garyb/purescript-indexed-monad
