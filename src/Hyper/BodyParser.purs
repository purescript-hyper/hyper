module Hyper.BodyParser where

import Control.Monad.Aff (makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Unit (Unit)
import Hyper.Conn (HTTP, Conn, RequestMiddleware)
import Hyper.Stream (Closed, Initial, Stream)

class BodyParser p t | p -> t where
  parse :: forall e req h. p
        -> RequestMiddleware
           e
           { bodyStream :: Stream Initial
           , headers :: { "content-type" :: String
                        , "content-length" :: String
                        | h
                        }
           | req
           }
           { bodyStream :: Stream Closed
           , headers :: { "content-type" :: String
                        , "content-length" :: String
                        | h
                        }
           , body :: t
           | req
           }

foreign import _parseBodyFromString :: forall e req res h t.
                                       -- Converter function.
                                       (String -> t)
                                       -- Conn to parse body from.
                                    -> Conn
                                       { bodyStream :: Stream Initial
                                       , headers :: { "content-type" :: String
                                                    , "content-length" :: String
                                                    | h
                                                    }
                                       | req
                                       }
                                       res
                                       -- Error callback.
                                    -> (Error -> Eff (http :: HTTP | e) Unit)
                                       -- Success callback.
                                    -> (Conn
                                        { bodyStream :: Stream Closed
                                        , headers :: { "content-type" :: String
                                                     , "content-length" :: String
                                                     | h
                                                     }
                                        , body :: t
                                        | req
                                        }
                                        res
                                       -> Eff (http :: HTTP | e) Unit)
                                       -- Effect of parsing.
                                    -> Eff (http :: HTTP | e) Unit

parseBodyFromString :: forall e req h t.
                       (String -> t)
                       -> RequestMiddleware
                       e
                       { bodyStream :: Stream Initial
                       , headers :: { "content-type" :: String
                                    , "content-length" :: String
                                    | h
                                    }
                       | req
                       }
                       { bodyStream :: Stream Closed
                       , headers :: { "content-type" :: String
                                    , "content-length" :: String
                                    | h
                                    }
                       , body :: t
                       | req
                       }
parseBodyFromString f conn = makeAff (_parseBodyFromString f conn)
