module Middlewarez.BodyParser where

import Middlewarez.Conn (Conn)
import Middlewarez.Stream (Closed, Initial, Stream)

class BodyParser p t where
  parse :: forall req res. p
        -> Conn { bodyStream :: Stream Initial | req } res
        -> Conn { bodyStream :: Stream Closed, body :: t | req } res

foreign import parseBodyFromString :: forall req res t. (String -> t)
                                   -> Conn { bodyStream :: Stream Initial | req } res
                                   -> Conn { bodyStream :: Stream Closed, body :: t | req } res
