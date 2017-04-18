# Changelog

* 0.7.3
  - Link to external packages in extensions docs
* 0.7.2
  - Improve docs template, and index mobile support
* 0.7.1
  - Deploy documentation for each tagged release
  - HTTPS for documentation site
  - No built docs in git repository
* 0.7.0
  - PureScript 0.11.x compatibility (breaking changes!)
* 0.6.0
  - Include early support for cookies and sessions
  - Add docs for new type-level routing API featuring the `Resource` construct
  - Use type classes instead of exposing record structure of `request` and
    `response`
    - The `Request` type class provides the `getRequestData` method
    - The `ReadableBody` type class provides overloaded ways of reading the
      request body as different types
    - Type signatures are generally much lighter and nicer to read!
* 0.5.0
  - Separate out
    [purescript-hyper-routing-server](https://github.com/owickstrom/purescript-hyper-routing-server)
    into a package, cleaning up the routing type API
  - Minor changes to docs
* 0.4.1
  - Fix issue with leaking type parameters in `runServer`, breaks backwards
    compatibility (type signatures changed)
* 0.4.0
  - Separate out
    [purescript-hyper-routing](https://github.com/owickstrom/purescript-hyper-routing)
    into a package
  - Migrate to Sphinx for documentation
  - Fix bug caused by `stream.writeString` on empty string
  - Improve content negotiation (now in separate routing lib)
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
