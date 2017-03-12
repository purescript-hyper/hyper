module Hyper.Node.Cookie
       (cookies
       ) where

import Prelude
import Data.NonEmpty as NonEmpty
import Data.StrMap as StrMap
import Hyper.Cookie as Cookie
import Control.IxMonad ((:>>=))
import Control.Monad.Error.Class (throwError)
import Data.Array (filter, foldMap, uncons, (:))
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(NonEmpty), (:|))
import Data.StrMap (StrMap)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Global (decodeURIComponent)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Middleware.Class (getConn, putConn)

toPair :: Array String -> Either String (Tuple String (Array String))
toPair kv =
  case kv of
    [key, value] ->
      pure (Tuple (decodeURIComponent key) [decodeURIComponent value])
    parts ->
      throwError ("Invalid cookie-pair: " <> joinWith " " parts)

splitPairs :: String → Either String (Array (Tuple String (Array String)))
splitPairs =
  split (Pattern ";")
  >>> map trim
  >>> filter ((/=) "")
  >>> map (split (Pattern "="))
  >>> map toPair
  >>> sequence

parseCookies :: String -> Either String (StrMap Cookie.Values)
parseCookies s =
  splitPairs s
  # map (foldMap toCookieMap)
  # map (StrMap.fromFoldableWith combineCookies)
  where
    toCookieMap (Tuple name values) =
      case uncons values of
        Just { head, tail } -> [Tuple name (NonEmpty head tail)]
        Nothing -> []

    combineCookies xs xs' =
      NonEmpty.head xs :| NonEmpty.head xs' : NonEmpty.tail xs <> NonEmpty.tail xs'

cookies
  :: forall m req res c.
     Monad m =>
     Middleware
     m
     (Conn { headers :: StrMap String | req } res { cookies :: Unit | c})
     (Conn { headers :: StrMap String | req } res { cookies :: Either String (StrMap Cookie.Values) | c})
     Unit
cookies =
  getConn :>>= \conn ->
  let cookies' = maybe (pure StrMap.empty) parseCookies (StrMap.lookup "cookie" conn.request.headers)
  in putConn conn { components { cookies = cookies' }}
