module Hyper.Node.Test where

import Node.Buffer as Buffer
import Control.Applicative (pure, class Applicative)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (singleton)
import Data.Function ((<<<))
import Data.Functor ((<$>))
import Data.Monoid (class Monoid)
import Data.Semigroup ((<>), class Semigroup)
import Hyper.Response (class Response)
import Node.Buffer (BUFFER, Buffer)
import Node.Encoding (Encoding(UTF8))

newtype TestResponseBody = TestResponseBody (Array Buffer)

instance stringTestResponseBody :: MonadAff (buffer :: BUFFER | e) m => Response m String TestResponseBody where
  toResponse body =
    TestResponseBody
    <$> singleton
    <$> liftAff (liftEff (Buffer.fromString body UTF8))

instance bufferTestResponseBody :: Applicative m => Response m Buffer TestResponseBody where
  toResponse = pure <<< TestResponseBody <<< singleton

instance semigroupBufferResponse :: Semigroup TestResponseBody where
  append (TestResponseBody chunks) (TestResponseBody chunks') =
    TestResponseBody (chunks <> chunks')

instance monoidBufferResponse :: Monoid TestResponseBody where
  mempty = TestResponseBody []
