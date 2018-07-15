-- | Parser for the `application/x-www-form-urlencoded` format, commonly used
-- | for query strings and POST request bodies.
module Hyper.Form.Urlencoded
       ( defaultOptions
       , Options
       , parseUrlencoded
       ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Global (decodeURIComponent)

toTuple :: Options -> Array String -> Either String (Tuple String (Maybe String))
toTuple opts kv =
  case kv of
    [key] ->
      pure (Tuple (decodeURIComponent key) Nothing)
    [key, value] ->
      let
        value' =
          if opts.replacePlus
            then
              replaceAll (Pattern "+") (Replacement " ") value
            else
              value
      in
        pure (Tuple (decodeURIComponent key) (Just (decodeURIComponent value')))
    parts ->
      throwError ("Invalid form key-value pair: " <> joinWith " " parts)

type Options = { replacePlus :: Boolean }

defaultOptions :: Options
defaultOptions = { replacePlus: true }

parseUrlencoded :: Options -> String -> Either String (Array (Tuple String (Maybe String)))
parseUrlencoded opts = split (Pattern "&")
                  >>> Array.filter (_ /= "")
                  >>> map (split (Pattern "="))
                  >>> map (toTuple opts)
                  >>> sequence
