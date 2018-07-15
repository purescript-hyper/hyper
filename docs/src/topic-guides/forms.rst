*****
Parsing incomming data
*****

Hyper doesn't provide any built in validation framework, but it makes
it quite easy for you to do the job by providing simple data type `Hyper.Form.Form`
which represent incomming `GET` or `POST` data. There is also some plumbing
implemented for parsing incomming urlencoded data into `Form` value.

Let's look at simple example in which we are going to do some simple validation of incoming data.

We first declare our data types, and some instance which we will need
later.

.. literalinclude:: FormSerialization.purs
   :language: haskell
   :start-after: start snippet datatypes
   :end-before: end snippet datatypes

In this example we use really simple approach to validation which only reports first encountered
error:

.. literalinclude:: FormSerialization.purs
   :language: haskell
   :start-after: start snippet parsing
   :end-before: end snippet parsing

Now we are ready to write our handler. We use ``parseOrder`` to get a
value of type ``Either String Order`` from ``Either String Form``,
where the ``String`` explains parsing errors. By pattern matching using
record field puns, we extract the ``beers`` and ``meal`` values, and respond
based on those values.

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


