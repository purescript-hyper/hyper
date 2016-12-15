module Hyper.Form (
  Form(..),
  parseForm
  ) where

import Prelude
import Control.Monad.Eff.Exception (error, Error)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic)
import Data.Monoid (class Monoid)
import Data.String (joinWith, Pattern(Pattern), split)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Global (decodeURIComponent)
import Hyper.Core (Middleware, Conn)

newtype Form = Form (Array (Tuple String String))

derive instance genericForm :: Generic Form
derive newtype instance eqForm :: Eq Form
derive newtype instance ordForm :: Ord Form
derive newtype instance showForm :: Show Form
derive newtype instance monoidForm :: Monoid Form

parseForm ∷ forall m req res c. Applicative m => Middleware m (Conn {body ∷ String | req} res c) (Conn {body ∷ Either Error Form | req} res c)
parseForm conn =
  let form = splitPairs conn.request.body
  in pure (conn { request = (conn.request { body = form }) })
  where
    toTuple :: Array String -> Either Error (Tuple String String)
    toTuple kv =
      case kv of
        [key, value] → Right (Tuple (decodeURIComponent key) (decodeURIComponent value))
        parts        → Left (error ("Invalid form key-value pair: " <> joinWith " " parts))
    splitPair = split (Pattern "=")
    splitPairs ∷ String → Either Error Form
    splitPairs = (<$>) Form
                 <<< sequence
                 <<< map toTuple
                 <<< map splitPair
                 <<< split (Pattern "&")
