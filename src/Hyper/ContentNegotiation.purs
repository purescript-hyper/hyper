-- This module implements, or aims to implement, Content Negotation, as
-- described in <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html>.
--
-- It needs more tests, and probably contains a lot of bugs. Also there's
-- some parts of the spec not implemented yet, commented as TODOs.
module Hyper.ContentNegotiation
       ( MediaRangeType(..)
       , MediaRange(..)
       , Q
       , AcceptParams(..)
       , AcceptEntry(..)
       , AcceptHeader(..)
       , acceptAll
       , parseAcceptHeader
       , negotiateContent
       ) where

import Prelude
import Data.Array as Array
import Data.Int as Int
import Data.List as List
import Data.Map as Map
import Control.Monad.Error.Class (throwError)
import Data.Array (uncons)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), split, trim)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

data MediaRangeType
  = Literal String
  | Wildcard

derive instance eqMediaRangeType :: Eq MediaRangeType
derive instance genericMediaRangeType :: Generic MediaRangeType
instance showMediaRangeType :: Show MediaRangeType where show = gShow

-- TODO: Support for parameters
data MediaRange = MediaRange MediaRangeType MediaRangeType

derive instance eqMediaRange :: Eq MediaRange
derive instance genericMediaRange :: Generic MediaRange
instance showMediaRange :: Show MediaRange where show = gShow

-- TODO: Add support for accept-extension
type Q = Number
data AcceptParams = AcceptParams Q (Map String String)

derive instance eqAcceptParams :: Eq AcceptParams

instance showAcceptParams :: Show AcceptParams where
  show (AcceptParams q ps) =
    "AcceptParams (" <> show q <> ") (" <> show ps <> ")"

data AcceptEntry = AcceptEntry MediaRange AcceptParams

derive instance eqAcceptEntry :: Eq AcceptEntry
instance showAcceptEntry :: Show AcceptEntry where
  show (AcceptEntry range params) =
    "AcceptEntry (" <> show range <> ") (" <> show params <> ")"

newtype AcceptHeader = AcceptHeader (Array AcceptEntry)

derive instance eqAcceptHeader :: Eq AcceptHeader
derive instance newtypeAcceptHeader :: Newtype AcceptHeader _

instance showAcceptHeader :: Show AcceptHeader where
  show h = "AcceptHeader (" <> show (unwrap h) <> ")"

acceptAll :: AcceptHeader
acceptAll = AcceptHeader [ AcceptEntry
                           (MediaRange Wildcard Wildcard)
                           (AcceptParams 1.0 Map.empty)
                         ]

parseAcceptHeader :: String -> Either String AcceptHeader
parseAcceptHeader x = parseHeader x
  where
    parseHeader s =
      split (Pattern ",") s
      # map trim
      # traverse parseEntry
      # map AcceptHeader
    parseEntry s =
      case uncons (map trim (split (Pattern ";") s)) of
        Just { head, tail } -> do
          range <- parseRange head
          params <- Map.fromFoldable <$> traverse parseParam tail
          let q = Map.lookup "q" params >>= Int.fromString
                  # map Int.toNumber
                  # fromMaybe 1.0
          pure (AcceptEntry range (AcceptParams q params))
        Nothing ->
          throwError ("Invalid Accept header entry: " <> s)
    parseParam s = do
      case map trim (split (Pattern "=") s) of
        [name, value] -> pure (Tuple name value)
        _ -> throwError ("Invalid parameter: " <> s)
    parseRange s =
      case split (Pattern "/") s of
        [type', subType] ->
          pure (MediaRange (parseType type') (parseType subType))
        _ ->
          throwError ("Invalid Accept header range: " <> s)
    parseType =
      case _ of
        "*" -> Wildcard
        l -> Literal l

sortEntriesByQuality
  :: Array AcceptEntry
  -> Array AcceptEntry
sortEntriesByQuality = Array.sortBy comparison
  where
    comparison
      -- TODO: Also compare specificity.
      (AcceptEntry _ (AcceptParams q1 _))
      (AcceptEntry _ (AcceptParams q2 _)) = compare q1 q2

matching :: forall r. AcceptEntry -> Tuple MediaType r -> Boolean
matching (AcceptEntry (MediaRange type' subType) _params) (Tuple (MediaType mt) _) =
  case split (Pattern "/") mt of
    [mtType, mtSubType] ->
      mtType `matchesRangeType` type' && mtSubType `matchesRangeType` subType
    _ ->
      true
  where
    matchesRangeType :: String -> MediaRangeType -> Boolean
    matchesRangeType s =
      case _ of
        Literal l -> s == l
        Wildcard -> true

negotiateContent
  :: forall r.
     AcceptHeader
  -> NonEmptyList (Tuple MediaType r)
  -> Maybe (Tuple MediaType r)
negotiateContent (AcceptHeader entries) responses =
  Array.head matches
  where
    entries' = sortEntriesByQuality entries
    responseList = toList responses
    matches = do
      entry <- entries
      case List.find (matching entry) responseList of
        Just p -> [p]
        Nothing -> []
