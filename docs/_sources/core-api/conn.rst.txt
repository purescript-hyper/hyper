****
Conn
****

A *Conn*, short for "connection", models the entirety of a connection
between the HTTP server and the user agent, both request and response.

.. code-block:: haskell

    type Conn req res components =
      { request :: req
      , response :: res
      , components :: components
      }

The ``request`` and ``response`` hold the values representing the HTTP
request and response, respectively. The purpose of the ``components``
field, however, is not that obvious. It is used for things not directly
related to the HTTP, but nonetheless related to the act of responding to
the HTTP request. A middleware can add information the Conn using
components, like providing authentication or authorization values. The
types of these components then becomes part of the Conn type, and you
get compile-time guarantees when using the provided components.

