*****
Forms
*****

When working with form data, we often want to serialize and deserialize
forms as custom data types, instead of working with the key-value pairs
directly. The ``ToForm`` and ``FromForm`` type classes abstracts
serialization and deserialization to form data, respectively.

We first declare our data types, and some instance which we will need
later.

.. literalinclude:: FormSerialization.purs
   :language: haskell
   :start-after: start snippet datatypes
   :end-before: end snippet datatypes

In this example we will only deserialize forms, and thus we only need
the ``FromForm`` instance.

.. literalinclude:: FormSerialization.purs
   :language: haskell
   :start-after: start snippet parsing
   :end-before: end snippet parsing

Now we are ready to write our handler. We use ``parseFromForm`` to get a
value of type ``Either String Order``, where the ``String`` explains
parsing errors. By pattern matching using record field puns, we extract
the ``beers`` and ``meal`` values, and respond based on those values.

.. literalinclude:: FormSerialization.purs
   :language: haskell
   :start-after: start snippet onPost
   :end-before: end snippet onPost

Let's try this server out at the command line.

.. code-block:: bash

   $ curl -X POST -d 'beers=6' http://localhost:3000
   Missing field: meal
   $ curl -X POST -d 'meal=Vegan&beers=foo' http://localhost:3000
   Invalid number: foo
   $ curl -X POST -d 'meal=Omnivore&beers=6' http://localhost:3000
   Sorry, we do not serve meat here.
   $ curl -X POST -d 'meal=Vegetarian&beers=6' http://localhost:3000
   One Vegetarian meal and 6 beers coming up!


