module Hyper.Conn where

type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }
