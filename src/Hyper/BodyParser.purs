module Hyper.BodyParser where

import Prelude
import Control.Monad.Aff (makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Right, Left))
import Hyper.Conn (HTTP, Conn, RequestMiddleware)
import Hyper.Stream (Closed, Initial, Stream)

class BodyParser p t | p -> t where
  parse :: forall e req h. p
        -> RequestMiddleware
           e
           -- Input:
           { body :: Stream Initial
           , headers :: { "content-type" :: String
                        , "content-length" :: String
                        | h
                        }
           | req
           }
           { headers :: { "content-type" :: String
                        , "content-length" :: String
                        | h
                        }
           , body :: t
           | req
           }

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
                                  -> (Error -> Eff (http :: HTTP | e) Unit)
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
                                      -> Eff (http :: HTTP | e) Unit)
                                     -- Effect of parsing.
                                  -> Eff (http :: HTTP | e) Unit

parseBodyFromString :: forall e req h t.
                       (String -> Either Error t)
                       -> RequestMiddleware
                       e
                       { body :: Stream Initial
                       , headers :: { "content-type" :: String
                                    , "content-length" :: String
                                    | h
                                    }
                       | req
                       }
                       { body :: t
                       , headers :: { "content-type" :: String
                                    , "content-length" :: String
                                    | h
                                    }
                       | req
                       }
parseBodyFromString f conn = do
  conn' ← makeAff (_parseBodyAsString conn)
  body ← case f conn'.request.body of
           Left err → throwError err
           Right body → pure body
  pure (conn { request = (conn'.request { body = body}) })
