********************
Request Body Reading
********************

The ``RequestBodyReader`` type class has one operation, ``readBody``,
which supports different servers to provide different types of request
body values.

.. code-block:: haskell

    class RequestBodyReader r m b | r -> b where
      readBody
        :: forall req res c.
           Middleware
           m
           (Conn { body :: r | req } res c)
           (Conn { body :: r | req } res c)
           b

Given that there is an instance for the body ``b``, and the return type
``r``, we can use this middleware together with other middleware, like
so:

.. literalinclude:: ReadBody.purs
   :language: haskell
   :start-after: start snippet onPost
   :end-before: end snippet onPost
