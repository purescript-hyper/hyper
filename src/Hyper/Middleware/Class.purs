module Hyper.Middleware.Class where

import Control.IxMonad (class IxMonad, ibind)
import Data.Unit (Unit)

class IxMonadMiddleware m where
  getConn ∷ ∀ i. m i i i
  putConn ∷ ∀ i o. o → m i o Unit

modifyConn ∷ ∀ m i o.
             (IxMonad m, IxMonadMiddleware m)
             ⇒ (i → o) → m i o Unit
modifyConn f = do
  c ← getConn
  putConn (f c)
  where bind = ibind
