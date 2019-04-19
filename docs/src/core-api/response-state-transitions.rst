**************************
Response State Transitions
**************************

The ``response`` field in the Conn is a value provided by the server backend.
Middleware often constrain the ``response`` field to be a value implementing the
``Response`` type class. This makes it possible to use response-writing
operations without depending on a specific server backend.

The state of a response is tracked in its last type parameter. This state
tracking, and the type-indexed middleware using the response, guarantee
correctness in response handling, preventing incorrect ordering of headers and
body writes, incomplete responses, or other such mistakes. Let us have a look
at the type signatures of some of response-writing functions in
``Hyper.Response``.

We see that ``headers`` takes a foldable collection of headers, and
gives back a middleware that, given a connection where headers are ready
to be written (``HeadersOpen``), writes all specified headers, writes
the separating CRLF before the HTTP body, and marks the state of the
response as being ready to write the body (``BodyOpen``).

.. code-block:: haskell

   headers
     :: forall t m req res b c
     . ( Foldable f
       , Monad m
       , Response res m b
       )
     => f Header
     -> Middleware
       m
       (Conn req (res HeadersOpen) c)
       (Conn req (res BodyOpen) c)
       Unit

To be used in combination with ``headers``, the ``respond`` function
takes some ``ResponseWritable b m r``, and gives back a middleware that, given
a connection *where all headers have been written*, writes a response, and
*marks the state of the response as ended*.

.. code-block:: haskell

   respond
     :: forall m r b req res c
      . ( Monad m
        , ResponseWritable b m r
        , Response res m b
        )
     => r
     -> Middleware
        m
        (Conn req (res BodyOpen) c)
        (Conn req (res ResponseEnded) c)
        Unit

The ``ResponseWritable`` type class describes types that can be written as
responses. It takes three type parameters, where ``b`` is the target type,
``m`` is a base monad for the Middleware returned, and ``r`` is the original
response type,

.. code-block:: haskell

   class ResponseWritable b m r where
     toResponse :: forall i. r -> Middleware m i i b

This mechanism allows servers to provide specific types for the response
body, along with instances for common response types. When using the
Node server, which has a response body type wrapping ``Buffer``, you can
still respond with a ``String`` or ``HTML`` value directly.

Aside from convenience in having a single function for most response
types and servers, the polymorphism of ``respond`` lets middleware be
decoupled from specific servers. It only requires an instance matching
the response type used by the middleware and the type required by the
server.
