module Middlewarez.Form (
  Form(..),
  formParser
  ) where

import Prelude
import Data.Generic (class Generic)
import Data.Monoid (class Monoid)
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import Global (decodeURIComponent)
import Middlewarez.BodyParser (parseBodyFromString, parse, class BodyParser)
import Middlewarez.Conn (RequestMiddleware)
import Middlewarez.Stream (Closed, Initial, Stream)

newtype Form = Form (Array (Tuple String String))

derive instance genericForm :: Generic Form
derive newtype instance eqForm :: Eq Form
derive newtype instance ordForm :: Ord Form
derive newtype instance showForm :: Show Form
derive newtype instance monoidForm :: Monoid Form

data FormParser = FormParser

instance bodyParserFormParser :: BodyParser FormParser Form where
  parse _ = parseBodyFromString splitEntries
    where
      toTuple :: Array String -> Tuple String String
      toTuple [key, value] = Tuple (decodeURIComponent key) (decodeURIComponent value)
      toTuple _ = Tuple "omg" "no" -- TODO: Implement error handling in body parsers
      splitEntry = split (Pattern "=")
      splitEntries = Form <<< map toTuple <<< map splitEntry <<< split (Pattern "&")


formParser :: forall e req.
              RequestMiddleware
              e
              { bodyStream :: Stream Initial
              , headers :: { "content-type" :: String
                           , "content-length" :: String
                           }
              | req
              }
              { bodyStream :: Stream Closed
              , headers :: { "content-type" :: String
                           , "content-length" :: String
                           }
              , body :: Form
              | req
              }
formParser = parse FormParser
