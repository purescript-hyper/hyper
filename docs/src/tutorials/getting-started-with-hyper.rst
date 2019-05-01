**************************
Getting Started with Hyper
**************************

Welcome to `Getting Started with Hyper`! The purpose of this tutorial is for
you to get a minimal Hyper project up and running. It assumes only that you
have working PureScript development environment, with ``node``, ``psc``,
``pulp``, and ``bower`` installed. If you do not have those tools already,
follow the installation steps described in `Getting Started
<https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md>`__
in the PureScript documentation.

.. note::

   Hyper 0.9.0 requires PureScript `version 0.12.2
   <https://github.com/purescript/purescript/releases/tag/v0.12.2>`__ or
   higher.

Start off by generating an empty project by entering the following commands in
your terminal:

.. code-block:: bash

   mkdir hello-hyper
   cd hello-hyper
   pulp init

Then install ``purescript-hyper``, and add it as a project dependency, by
running:

.. code-block:: bash

   bower i purescript-hyper --save

You now have what you need to write a server. Edit ``src/Main.purs`` to look
like this:

.. code-block:: haskell

   module Main where

   import Prelude
   import Control.Monad.Indexed ((:*>))
   import Effect (Effect)
   import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
   import Hyper.Response (closeHeaders, respond, writeStatus)
   import Hyper.Status (statusOK)

   main :: Effect Unit
   main =
     let app = writeStatus statusOK
               :*> closeHeaders
               :*> respond "Hello, Hyper!"
     in runServer defaultOptionsWithLogging {} app

The `main` function defines a value `app`, which is a
:doc:`/core-api/middleware`, responding with the HTTP status "200 OK", no extra
headers, and "Hello, Hyper!" as the response body. The use of `runServer`
creates a NodeJS server running our middleware.

Now build and run the program:

.. code-block:: bash

   pulp run

You should see output similar to the following:

.. code-block:: bash

   * Building project in /tmp/hello-hyper
   * Build successful.
   Listening on http://localhost:3000

Open http://localhost:3000 in your web browser, and you should see "Hello,
Hyper!" rendered. Congratulations, you have written your first web server using
Hyper!
