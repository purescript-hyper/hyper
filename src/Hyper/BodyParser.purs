module Hyper.BodyParser where

import Prelude
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Right, Left))
import Hyper.Core (Conn, Middleware)
import Hyper.Stream (Initial, Stream)

class BodyParser p t m | p -> t where
  parse :: forall req res c h. p
        -> Middleware
           m
           (Conn
            { body :: Stream Initial
            , headers :: { "content-type" :: String
                         , "content-length" :: String
                         | h
                         }
            | req
            }
            res
            c)
           (Conn
            { headers :: { "content-type" :: String
                         , "content-length" :: String
                         | h
                         }
            , body :: t
            | req
            }
            res
            c)

foreign import _parseBodyAsString :: forall e req res c h.
                                     -- Conn to parse body from.
                                     Conn
                                     { body :: Stream Initial
                                     , headers :: { "content-type" :: String
                                                  , "content-length" :: String
                                                  | h
                                                  }
                                     | req
                                     }
                                     res
                                     c
                                     -- Error callback.
                                  -> (Error -> Eff e Unit)
                                     -- Success callback.
                                  -> (Conn
                                      { headers :: { "content-type" :: String
                                                   , "content-length" :: String
                                                   | h
                                                   }
                                      , body :: String
                                      | req
                                      }
                                      res
                                      c
                                      -> Eff e Unit)
                                     -- Effect of parsing.
                                  -> Eff e Unit

parseBodyFromString :: forall e req res h c t.
                       (String -> Either Error t)
                       -> Middleware
                       (Aff e)
                       (Conn
                        { body :: Stream Initial
                        , headers :: { "content-type" :: String
                                     , "content-length" :: String
                                     | h
                                     }
                        | req
                        }
                        res
                        c)
                       (Conn
                        { body :: t
                        , headers :: { "content-type" :: String
                                     , "content-length" :: String
                                     | h
                                     }
                        | req
                        }
                        res
                        c)
parseBodyFromString f conn = do
  c ← makeAff (_parseBodyAsString conn)
  body ← case f c.request.body of
           Left err → throwError err
           Right body → pure body
  pure (c { request = (c.request { body = body}) })
