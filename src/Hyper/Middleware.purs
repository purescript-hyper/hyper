module Hyper.Middleware where

import Prelude
import Control.IxMonad (class IxMonad, ibind, ipure)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Tuple (Tuple(..), snd)
import Hyper.Middleware.Class (class IxMonadMiddleware)

newtype Middleware m i o a = Middleware (i → m (Tuple a o))

runMiddleware ∷ ∀ m i o a. Middleware m i o a → i → m (Tuple a o)
runMiddleware (Middleware m) x = m x

evalMiddleware ∷ ∀ m i o a. Functor m ⇒ Middleware m i o a → i → m o
evalMiddleware a s = map snd (runMiddleware a s)

instance ixMonadMiddlewareMiddleware ∷ Applicative m ⇒ IxMonadMiddleware (Middleware m) where
  getConn = Middleware $ \c → pure (Tuple c c)
  putConn c = Middleware $ \_ → pure (Tuple unit c)

instance ixMonadMiddleware ∷ Monad m ⇒ IxMonad (Middleware m) where
  ipure x = Middleware $ \s → pure (Tuple x s)
  ibind (Middleware ma) f =
    Middleware $ \s →
      ma s >>= \(Tuple x s') →
      case f x of
        Middleware a → a s'

{-
instance monadEffMiddleware ∷ MonadEff e m ⇒ IxMonadEff e (Middleware m) where
  iliftEff e = Middleware $ \s → do
    x ← liftEff (runIxEff e)
    pure (Tuple x s)
-}

instance functorMiddleware ∷ Monad m => Functor (Middleware m i i) where
  map f a =
    Middleware $ \s →
      runMiddleware a s >>= \(Tuple a' s') →
      pure (Tuple (f a') s')


instance applyMiddleware ∷ Monad m => Apply (Middleware m i i) where
  apply f a =
    Middleware $ \s →
      runMiddleware f s >>= \(Tuple f' s') →
      runMiddleware a s' >>= \(Tuple a' s'') →
      pure (Tuple (f' a') s'')

instance applicativeMiddleware ∷ Monad m => Applicative (Middleware m i i) where
  pure = ipure

instance bindMiddleware ∷ Monad m ⇒ Bind (Middleware m i i) where
  bind = ibind

instance monadMiddleware ∷ (Monad m, Applicative m) => Monad (Middleware m i i)

instance monadEffMiddleware ∷ MonadEff e m ⇒ MonadEff e (Middleware m i i) where
  liftEff e = Middleware $ \s → do
    x ← liftEff e
    pure (Tuple x s)

-- TODO: Can this be written as an instance of MonadTrans? Can't get the type
-- arguments to line up proplerly...
lift' ∷ ∀ m i a. Monad m ⇒ m a → Middleware m i i a
lift' a = Middleware $ \s → do
  x ← a
  pure (Tuple x s)
