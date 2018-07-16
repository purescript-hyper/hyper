module Hyper.Status where

import Prelude

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Record.Extra (compareRecord)

newtype Status = Status { code :: Int, reasonPhrase :: String }

status :: Int -> String -> Status
status code reasonPhrase = Status { code: code, reasonPhrase: reasonPhrase }

derive instance newtypeStatus :: Newtype Status _
derive instance genericStatus :: Generic Status _
derive newtype instance eqStatus :: Eq Status
instance ordStatus :: Ord Status where
  compare s1 s2 = compareRecord (unwrap s1) (unwrap s2)
instance showStatus :: Show Status where show = genericShow

statusOK :: Status
statusOK = status 200 "OK"

statusCreated :: Status
statusCreated = status 201 "Created"

statusFound :: Status
statusFound = status 302 "Found"

statusBadRequest :: Status
statusBadRequest = status 400 "Bad Request"

statusUnauthorized :: Status
statusUnauthorized = status 401 "Unauthorized"

statusForbidden :: Status
statusForbidden = status 403 "Forbidden"

statusNotFound :: Status
statusNotFound = status 404 "Not Found"

statusMethodNotAllowed :: Status
statusMethodNotAllowed = status 405 "Method Not Allowed"

statusNotAcceptable :: Status
statusNotAcceptable = status 406 "Not Acceptable"
