module Hyper.Form
       ( Form(..)
       , optional
       , required
       , parseForm
       , fromForm
       , toForm
       , class FromForm
       , class ToForm
       , parseFromForm
       ) where

import Prelude
import Data.Tuple as Tuple
import Control.IxMonad (ibind, ipure, (:>>=))
import Control.Monad.Error.Class (throwError)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.MediaType (MediaType(MediaType))
import Data.MediaType.Common (applicationFormURLEncoded)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (lookup, StrMap)
import Data.String (split, joinWith, Pattern(Pattern))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Global (decodeURIComponent)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Middleware.Class (getConn)
import Hyper.Request (class RequestBodyReader, readBody)

newtype Form = Form (Array (Tuple String (Maybe String)))

derive instance newtypeForm :: Newtype Form _
derive instance genericForm :: Generic Form
derive newtype instance eqForm :: Eq Form
derive newtype instance ordForm :: Ord Form
derive newtype instance showForm :: Show Form
derive newtype instance monoidForm :: Monoid Form


optional :: String -> Form -> Maybe String
optional key = do
  unwrap
  >>> Tuple.lookup key
  >>> flip bind id


required :: String -> Form -> Either String String
required key =
  optional key
  >>> maybe (throwError ("Missing field: " <> key)) pure


parseContentMediaType :: String -> Maybe MediaType
parseContentMediaType = split (Pattern ";")
                        >>> head
                        >>> map MediaType


toTuple :: Array String -> Either String (Tuple String (Maybe String))
toTuple kv =
  case kv of
    [key] ->
      pure (Tuple (decodeURIComponent key) Nothing)
    [key, value] ->
      pure (Tuple (decodeURIComponent key) (Just (decodeURIComponent value)))
    parts ->
      throwError ("Invalid form key-value pair: " <> joinWith " " parts)


splitPairs :: String → Either String (Array (Tuple String (Maybe String)))
splitPairs = split (Pattern "&")
             >>> map (split (Pattern "="))
             >>> map toTuple
             >>> sequence

parseForm ∷ forall m req res c r.
            ( Monad m
            , RequestBodyReader r m String
            ) =>
            Middleware
            m
            (Conn { body :: r
                  , headers :: StrMap String
                  | req
                  } res c)
            (Conn { body ∷ r
                  , headers :: StrMap String
                  | req
                  }
                  res
                  c)
            (Either String Form)
parseForm = do
  conn <- getConn
  body <- readBody
  case lookup "content-type" conn.request.headers >>= parseContentMediaType of
    Nothing ->
      ipure (Left "Missing or invalid content-type header.")
    Just mediaType | mediaType == applicationFormURLEncoded ->
      ipure (Form <$> splitPairs body)
    Just mediaType ->
      ipure (Left ("Cannot parse media of type: " <> show mediaType))
  where bind = ibind


class ToForm a where
  toForm ∷ a → Form


class FromForm a where
  fromForm ∷ Form → Either String a


parseFromForm
  ∷ forall m req res c r a.
    ( Monad m
    , RequestBodyReader r m String
    , FromForm a
    )
  => Middleware
     m
     (Conn { body :: r
           , headers :: StrMap String
           | req
           } res c)
     (Conn { body ∷ r
           , headers :: StrMap String
           | req
           }
           res
           c)
     (Either String a)
parseFromForm =
  parseForm :>>=
  case _ of
    Left err -> ipure (Left err)
    Right form -> ipure (fromForm form)
