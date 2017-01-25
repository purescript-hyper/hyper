module Hyper.Routing.PathPiece where

import Data.Int as Int
import Control.Category (id)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)

class ToPathPiece x where
  toPathPiece :: x -> String

instance toPathPieceString :: ToPathPiece String where
  toPathPiece = id

instance toPathPieceInt :: ToPathPiece Int where
  toPathPiece = show

class FromPathPiece x where
  fromPathPiece :: String -> Either String x

instance fromPathPieceString :: FromPathPiece String where
  fromPathPiece = Right

instance fromPathPieceInt :: FromPathPiece Int where
  fromPathPiece s =
    case Int.fromString s of
      Just n -> Right n
      Nothing -> Left ("Invalid Int: " <> s)
