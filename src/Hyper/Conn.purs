module Hyper.Conn (
  Conn,
  HTTP
  ) where

foreign import data HTTP :: !

type Conn req res components = { request :: req
                               , response :: res
                               , components :: components
                               }
