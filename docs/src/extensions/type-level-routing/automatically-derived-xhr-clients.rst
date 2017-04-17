*********************************
Automatically Derived XHR Clients
*********************************

As we represent routing as types, we can derive XHR clients from those
types. In a client-side application, for example one written using
`Pux <https://www.purescript-pux.org>`__ or
`Halogen <https://github.com/slamdata/purescript-halogen>`__, we
use the
`purescript-hyper-routing-xhr <https://github.com/owickstrom/purescript-hyper-routing-xhr>`__
library to derive such client functions. We get functions with
type-safe parameters and encoding/decoding, using our routed types, and
results in the `Aff <https://github.com/slamdata/purescript-aff>`__
monad, which are easily integrated into most frameworks.
