-- | Parser for the `application/x-www-form-urlencoded` format, commonly used
-- | for query strings and POST request bodies.
module Hyper.Form.Urlencoded
       ( parseUrlencoded
       ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (split, joinWith, Pattern(Pattern))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Global.Unsafe (unsafeDecodeURIComponent)

toTuple :: Array String -> Either String (Tuple String (Maybe String))
toTuple kv =
  case kv of
    [key] ->
      pure (Tuple (unsafeDecodeURIComponent key) Nothing)
    [key, value] ->
      pure (Tuple (unsafeDecodeURIComponent key) (Just (unsafeDecodeURIComponent value)))
    parts ->
      throwError ("Invalid form key-value pair: " <> joinWith " " parts)


parseUrlencoded :: String → Either String (Array (Tuple String (Maybe String)))
parseUrlencoded = split (Pattern "&")
                  >>> Array.filter (_ /= "")
                  >>> map (split (Pattern "="))
                  >>> map toTuple
                  >>> sequence
