module Hyper.Middleware.QualifiedDo where

import Control.Monad.Indexed (class IxBind, ibind)

bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

