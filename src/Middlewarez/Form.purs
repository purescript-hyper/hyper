module Middlewarez.Form (
  Form(..),
  formParser
  ) where

import Prelude
import Middlewarez.BodyParser (parse, unsafeParseBody, class BodyParser)
import Middlewarez.Conn (RequestMiddleware)
import Middlewarez.Stream (Closed, Initial, Stream)

data Form = Form

instance showForm :: Show Form where
  show _ = "Form"

instance eqForm :: Eq Form where
  eq Form Form = true

data FormParser = FormParser

instance bodyParserFormParser :: BodyParser FormParser Form where
  parse _ = unsafeParseBody (\_ -> Form)

formParser :: forall req.
              RequestMiddleware { bodyStream :: Stream Initial | req }
                                { bodyStream :: Stream Closed , body :: Form | req }
formParser = parse FormParser
