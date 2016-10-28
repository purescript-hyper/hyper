module Middlewarez.Conn where

import Prelude
import Middlewarez.Stream (stdin, Closed, Initial, Stream)

type Conn r = r -- Might add default labels here later

class BodyParser p t where
  parse :: forall r. p
        -> Conn { bodyStream :: Stream Initial | r }
        -> Conn { bodyStream :: Stream Closed, body :: t | r}

data Form = Form

instance showForm :: Show Form where
  show _ = "Form"

instance eqForm :: Eq Form where
  eq Form Form = true


data FormParser = FormParser

foreign import unsafeParseBody :: forall r t. (String -> t)
                               -> Conn { bodyStream :: Stream Initial | r }
                               -> Conn { bodyStream :: Stream Closed, body :: t | r }

instance bodyParserFormparser :: BodyParser FormParser Form where
  parse _ = unsafeParseBody (\_ -> Form)

type Middleware r r' = Conn r -> Conn r'

formParser :: forall r.
              Middleware { bodyStream :: Stream Initial | r }
                         { bodyStream :: Stream Closed , body :: Form | r }
formParser = parse FormParser

testing :: Conn { bodyStream :: Stream Closed
                , body :: Form
                }
testing = formParser ({ bodyStream: stdin })
