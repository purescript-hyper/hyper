module Middlewarez.Form (
  Form(..),
  formParser
  ) where

import Prelude
import Data.Generic (class Generic)
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple)
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
  parse _ = parseBodyFromString (\_ -> Form [])

formParser :: forall req.
              RequestMiddleware { bodyStream :: Stream Initial
                                , headers :: Unit
                                | req
                                }
                                { bodyStream :: Stream Closed
                                , headers :: Unit
                                , body :: Form
                                | req
                                }
formParser = parse FormParser
