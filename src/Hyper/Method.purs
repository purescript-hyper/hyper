module Hyper.Method where

import Prelude

data Method = GET | POST

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"
