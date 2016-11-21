module Hyper.Method where

import Data.Show (class Show)

data Method = GET | POST

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"
