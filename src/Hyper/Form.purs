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

import Control.Monad.Error.Class (throwError)
import Control.Monad.Indexed (ibind, ipure, (:>>=))
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.MediaType (MediaType(MediaType))
import Data.MediaType.Common (applicationFormURLEncoded)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Foreign.Object (lookup)
import Hyper.Conn (BodyRead, BodyUnread, Conn, kind ResponseState)
import Hyper.Form.Urlencoded (parseUrlencoded)
import Hyper.Middleware (Middleware)
import Hyper.Middleware.Class (getConn)
import Hyper.Request (class Request, class ReadableBody, getRequestData, readBody)

newtype Form = Form (Array (Tuple String (Maybe String)))

derive instance newtypeForm :: Newtype Form _
derive newtype instance eqForm :: Eq Form
derive newtype instance ordForm :: Ord Form
derive newtype instance showForm :: Show Form
derive newtype instance semigroupForm :: Semigroup Form
derive newtype instance monoidForm :: Monoid Form


optional :: String -> Form -> Maybe String
optional key = do
  unwrap
  >>> Tuple.lookup key
  >>> flip bind identity


required :: String -> Form -> Either String String
required key =
  optional key
  >>> maybe (throwError ("Missing field: " <> key)) pure


parseContentMediaType :: String -> Maybe MediaType
parseContentMediaType = split (Pattern ";")
                        >>> head
                        >>> map MediaType

parseForm ∷ forall m req (res :: ResponseState -> Type) comp (resState :: ResponseState)
  .  Monad m
  => Request req m
  => ReadableBody req m String
  => Middleware
      m
      (Conn req BodyUnread res resState comp)
      (Conn req BodyRead res resState comp)
      (Either String Form)
parseForm = do
  conn <- getConn
  { headers } <- getRequestData
  body <- readBody
  case lookup "content-type" headers >>= parseContentMediaType of
    Nothing ->
      ipure (Left "Missing or invalid content-type header.")
    Just mediaType | mediaType == applicationFormURLEncoded ->
      ipure (Form <$> parseUrlencoded body)
    Just mediaType ->
      ipure (Left ("Cannot parse media of type: " <> show mediaType))
  where bind = ibind


class ToForm a where
  toForm ∷ a → Form


class FromForm a where
  fromForm ∷ Form → Either String a


parseFromForm ∷ forall m req (res :: ResponseState -> Type) comp (resState :: ResponseState) a
  .  Monad m
  => Request req m
  => ReadableBody req m String
  => FromForm a
  => Middleware
     m
     (Conn req BodyUnread res resState comp)
     (Conn req BodyRead res resState comp)
     (Either String a)
parseFromForm =
  parseForm :>>=
  case _ of
    Left err -> ipure (Left err)
    Right form -> ipure (fromForm form)
