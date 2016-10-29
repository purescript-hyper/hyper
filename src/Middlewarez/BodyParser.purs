module Middlewarez.BodyParser where

import Control.Monad.Aff (makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Unit (Unit)
import Middlewarez.Conn (HTTP, Conn, RequestMiddleware)
import Middlewarez.Stream (Closed, Initial, Stream)

class BodyParser p t | p -> t where
  parse :: forall e req. p
        -> RequestMiddleware
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
           , body :: t
           | req
           }

foreign import _parseBodyFromString :: forall e req res t.
                                       (String -> t)
                                    -> (Conn
                                        { bodyStream :: Stream Initial
                                        , headers :: { "content-type" :: String
                                                     , "content-length" :: String
                                                     }
                                        | req
                                        }
                                        res)
                                    -> (Error -> Eff (http :: HTTP | e) Unit)
                                    -> (Conn
                                        { bodyStream :: Stream Closed
                                        , headers :: { "content-type" :: String
                                                     , "content-length" :: String
                                                     }
                                        , body :: t
                                        | req
                                        }
                                        res
                                       -> Eff (http :: HTTP | e) Unit)
                                    -> Eff (http :: HTTP | e) Unit

parseBodyFromString :: forall e req t.
                       (String -> t)
                       -> RequestMiddleware
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
                       , body :: t
                       | req
                       }
parseBodyFromString f conn = makeAff (_parseBodyFromString f conn)
