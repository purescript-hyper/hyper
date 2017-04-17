************
Introduction
************

Composing middleware in NodeJS is a risky business. They mutate the HTTP request
and response objects freely, and are often dependent on each others'
side-effects. There are no guarantees that you have stacked the middleware
functions in a sensible order, and it is often the case, in the author's
experience, that misconfigured middleware takes a lot of time and effort to
debug.

Goals
#####

The goal of *Hyper* is to make use of row polymorphism and other tasty
type system features in PureScript to enforce correctly stacked
middleware in HTTP server applications. All effects of middleware
should be reflected in the types to ensure that common mistakes cannot be made.
A few examples of such mistakes could be:

* Incorrect ordering of header and body writing
* Writing incomplete responses
* Writing multiple responses
* Trying to consume a non-parsed request body
* Consuming a request body parsed as the wrong type
* Incorrect ordering of, or missing, error handling middleware
* Incorrect ordering of middleware for sessions, authentication, authorization
* Missing authentication and/or authorization checks
* Linking, in an HTML anchor, to a resource that is not routed
* Posting, in an HTML form, to a resource that is not routed

Hyper aims to solve these issues, in part through its Core API for middleware,
but also through a number of extensions for building safely composable and
maintainable web applications.

Contributing
############

While Hyper is currently an experiment, and in constant flux, you are
welcome to contribute. Please post ideas and start discussions using
`the issue tracker on
GitHub <https://github.com/owickstrom/hyper/issues>`__. You can also
contact `Oskar Wickström <https://wickstrom.tech/about.html>`__ directly
for design discussions. If this project grows, we can setup a mailing
list, or some other means of communication.

Please note that sending pull requests without first discussing the
design is probably a waste of time, if not only fixing simple things
like typos.
