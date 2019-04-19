*******
Servers
*******

Although Hyper middleware can be applied directly to Conn values using
``runMiddleware``, you likely want a *server* to run your middleware.
Hyper tries to be as open as possible when it comes to servers -- your
application, and the middleware it depends on, should not be tied to a
specific server. This allows for greater reuse and the ability to test
entire applications without running the "real" server. Currently Hyper
bundles a NodeJS server, described in :doc:`/topic-guides/nodejs`, as well as a
test server, described in :doc:`/topic-guides/testing`.
