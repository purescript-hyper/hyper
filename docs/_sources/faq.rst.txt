**************************
Frequently Asked Questions
**************************

This document lists some of the questions and considerations about Hyper that
come up frequently, and answers that hopefully serves future readers well.
Please, do not regard this as a complete rationale of the project, but rather
a casual summary of discussions that have been held in the past.

Why PureScript, and not Haskell?
--------------------------------

This project started out as a curiosity around expression statically typed
middleware using `extensible records
<https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/>`__.
While Haskell has a record construct, extensible records is not a built-in
feature, and the libraries implementing extensible records for Haskell seemed
too clunky to build a library upon. PureScript's extensible records, and row
typing, was a very good match for middleware typing, and the cognitive
overhead and expressiveness was reasonable.

Another concern, which might not be shared by all readers, is the NodeJS
deployment target. While the project author would love to see more support for
Haskell deployments in PaaS solutions, the current situation seem to favour
deployment on NodeJS, JVM, Ruby, and Python. Also, many companies and
developers might have invested in NodeJS infrastructure and libraries of their
own, and so PureScript provides a gradual migration path to statically typed
functional programming in web development.

The third point to consider is that PureScript is gaining traction on the
client side, competing, and perhaps living in symbiosis, with frontend
frameworks like React, Angular, and Ember. Having the option to share data
types between client and server, and write them both in a language like
PureScript, is something Hyper emphasizes.
