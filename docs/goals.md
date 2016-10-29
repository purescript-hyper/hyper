# Goals

Composing middleware in NodeJS is a risky business. They mutate the HTTP request
and response objects freely, and are often dependent on each others'
side-effects. There are no guarantees that you have stacked the middleware
functions in a sensible order, and it is often the case, in my experience, that
misconfigured middleware takes a lot of time and effort to debug.

The goals of this little hack, called *Hyper*, is to make use of row
polymorphism and other tasty type system features in PureScript to enforce
correctly stacked middleware in HTTP server applications. All effects of
middleware should be reflected in the types to ensure that otherwise common
mistakes cannot be made. A few examples could be:

* Trying to consume a non-parsed request body
* Consuming a request body parsed as the wrong type
* Overwriting headers
* Writing multiple responses
* Incorrect ordering of error handling middleware
* Incorrect ordering of middleware for sessions, authentication, authorization
* Missing authentication and/or authorization checks

Can we, using the PureScript type system, eradicate this class of errors? Let's
find out!
