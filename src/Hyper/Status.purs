module Hyper.Status where

import Data.Eq (class Eq)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Show (class Show)

newtype Status = Status { code :: Int, reasonPhrase :: String }

status :: Int -> String -> Status
status code reasonPhrase = Status { code: code, reasonPhrase: reasonPhrase }

derive instance genericStatus :: Generic Status
derive instance newtypeStatus :: Newtype Status _

instance eqStatus :: Eq Status where eq = gEq
instance ordStatus :: Ord Status where compare = gCompare
instance showStatus :: Show Status where show = gShow

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
