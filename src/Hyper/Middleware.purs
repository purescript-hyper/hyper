module Hyper.Middleware where

import Prelude
import Control.IxMonad (class IxMonad, ibind, ipure)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Data.Tuple (Tuple(..), snd)
import Hyper.Middleware.Class (class IxMonadMiddleware)

newtype Middleware m i o a = Middleware (i -> m (Tuple a o))

runMiddleware :: forall m i o a. Middleware m i o a -> i -> m (Tuple a o)
runMiddleware (Middleware m) x = m x

evalMiddleware :: forall m i o a. Functor m ⇒ Middleware m i o a -> i -> m o
evalMiddleware a s = map snd (runMiddleware a s)

hoistMiddleware :: forall f g i o a. (f ~> g) -> Middleware f i o a -> Middleware g i o a
hoistMiddleware f (Middleware k) = Middleware (f <<< k)

instance ixMonadMiddlewareMiddleware :: Applicative m ⇒ IxMonadMiddleware (Middleware m) where
  getConn = Middleware $ \c -> pure (Tuple c c)
  putConn c = Middleware $ \_ -> pure (Tuple unit c)

instance ixMonadMiddleware :: Monad m ⇒ IxMonad (Middleware m) where
  ipure x = Middleware $ \s -> pure (Tuple x s)
  ibind (Middleware ma) f =
    Middleware $ \s ->
      ma s >>= \(Tuple x s') ->
      case f x of
        Middleware a -> a s'

instance functorMiddleware :: Monad m => Functor (Middleware m i i) where
  map f a =
    Middleware $ \s ->
      runMiddleware a s >>= \(Tuple a' s') ->
      pure (Tuple (f a') s')


instance applyMiddleware :: Monad m => Apply (Middleware m i i) where
  apply f a =
    Middleware $ \s ->
      runMiddleware f s >>= \(Tuple f' s') ->
      runMiddleware a s' >>= \(Tuple a' s'') ->
      pure (Tuple (f' a') s'')

instance applicativeMiddleware :: Monad m => Applicative (Middleware m i i) where
  pure = ipure

instance bindMiddleware :: Monad m ⇒ Bind (Middleware m i i) where
  bind = ibind

instance monadMiddleware :: (Monad m, Applicative m) => Monad (Middleware m i i)

instance monadEffMiddleware :: MonadEffect m ⇒ MonadEffect (Middleware m i i) where
  liftEffect e = Middleware $ \s -> do
    x <- liftEffect e
    pure (Tuple x s)

instance monadAffMiddleware :: MonadAff m ⇒ MonadAff (Middleware m i i) where
  liftAff e = Middleware $ \s -> do
    x <- liftAff e
    pure (Tuple x s)

-- TODO: Can this be written as an instance of MonadTrans? Can't get the type
-- arguments to line up properly...
lift' :: forall m i a. Monad m ⇒ m a -> Middleware m i i a
lift' a = Middleware $ \s -> do
  x <- a
  pure (Tuple x s)
