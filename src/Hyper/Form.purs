module Hyper.Form (
  Form(..),
  parseForm
  ) where

import Prelude
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Data.Array (head)
import Data.Either (Either)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.MediaType (MediaType(MediaType))
import Data.MediaType.Common (applicationFormURLEncoded)
import Data.Monoid (class Monoid)
import Data.StrMap (lookup, StrMap)
import Data.String (split, joinWith, Pattern(Pattern))
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


parseContentMediaType :: String -> Maybe MediaType
parseContentMediaType = split (Pattern ";")
                        >>> head
                        >>> map MediaType

toTuple :: Array String -> Either Error (Tuple String String)
toTuple kv =
  case kv of
    [key, value] ->
      pure (Tuple (decodeURIComponent key) (decodeURIComponent value))
    parts ->
      throwError (error ("Invalid form key-value pair: " <> joinWith " " parts))

splitPairs :: String → Either Error (Array (Tuple String String))
splitPairs = split (Pattern "&")
             >>> map (split (Pattern "="))
             >>> map toTuple
             >>> sequence

parseForm ∷ forall m req res c.
            Applicative m =>
            Middleware
            m
            (Conn { body ∷ String
                  , headers :: StrMap String
                  | req
                  } res c)
            (Conn { body ∷ Either Error Form
                  , headers :: StrMap String
                  | req
                  }
                  res
                  c)
parseForm conn =
  pure (conn { request = (conn.request { body = form }) })
  where
    form =
      case lookup "content-type" conn.request.headers >>= parseContentMediaType of
        Nothing ->
          throwError (error "Missing or invalid content-type header.")
        Just mediaType | mediaType == applicationFormURLEncoded ->
          Form <$> splitPairs conn.request.body
        Just mediaType ->
          throwError (error ("Cannot parse media of type: " <> show mediaType))
