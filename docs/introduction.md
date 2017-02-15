# Introduction

Welcome to the Hyper documentation! Hyper is an experimental middleware
architecture for HTTP servers written in PureScript.  It is also a breeding
ground for higher-level web server constructs, which tends to fall under the
"framework" category. I hope you find something useful or inspiring in this
project.

## Goals

Composing middleware in NodeJS is a risky business. They mutate the HTTP request
and response objects freely, and are often dependent on each others'
side-effects. There are no guarantees that you have stacked the middleware
functions in a sensible order, and it is often the case, in my experience, that
misconfigured middleware takes a lot of time and effort to debug.

The goal of *Hyper* is to make use of row polymorphism and other tasty
type system features in PureScript to enforce correctly stacked
middleware in HTTP server applications. All effects of middleware
should be reflected in the types to ensure that common mistakes cannot be made.
A few examples could be:

* Incorrect ordering header and body writing
* Writing incomplete responses
* Overwriting headers
* Writing multiple responses
* Trying to consume a non-parsed request body
* Consuming a request body parsed as the wrong type
* Incorrect ordering of error handling middleware
* Incorrect ordering of middleware for sessions, authentication, authorization
* Missing authentication and/or authorization checks
* Linking, in an HTML anchor, to a resource that is not routed
* Posting, in an HTML form, to a resource that is not routed

Can we use the PureScript type system to eradicate this class of errors? Let's
find out!
