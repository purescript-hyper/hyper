**********
Middleware
**********

A *middleware* is an *indexed monadic action* transforming one ``Conn``
to another ``Conn``. It operates in some base monad ``m``, and is
indexed by ``i`` and ``o``, the *input* and *output* Conn types of the
middleware action.

.. code-block:: haskell

    newtype Middleware m i o a = ...

The input and output type parameters are used to ensure that a Conn is
transformed, and that side-effects are performed, correctly, throughout
the middleware chain.

Being able to parameterize ``Middleware`` with some type ``m``, you can
customize the chain depending on the needs of your middleware and
handlers. Applications can use monad transformers to track state,
provide configuration, gather metrics, and much more, in the chain of
middleware.

Middleware are composed using ``ibind``, the indexed monadic version of
``bind``. The simplest way of composing middleware is by chaining them
with ``:*>``, from ``Control.Monad.Indexed``. See
`purescript-indexed-monad <https://pursuit.purescript.org/packages/purescript-indexed-monad/1.0.0>`__
for more information.

.. code-block:: haskell

    writeStatus statusOK
    :*> closeHeaders
    :*> respond "We're composing middleware!"

If you want to feed the return value of one middleware into another, use
``:>>=``, the infix operator alias for ``ibind``.

.. code-block:: haskell

    getUser :>>= renderUser

The *qualified do* syntax allows you to use ``ibind`` implicitly
instead of the regular ``bind``.

.. code-block:: haskell

    Middleware.do
      user <- getUser
      writeStatus statusOK
      closeHeaders
      respond ("User: " <> user.name)
