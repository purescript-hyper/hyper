module Hyper.Node.Server.Options
       ( defaultOptions
       , defaultOptionsWithLogging
       , Hostname(..)
       , Options(..)
       , Port(..)
       ) where

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Data.Newtype (class Newtype)
import Prelude

newtype Hostname = Hostname String
derive instance newtypeHostname :: Newtype Hostname _

newtype Port = Port Int
derive instance newtypePort :: Newtype Port _

type Options =
  { hostname :: Hostname
  , port :: Port
  , onListening :: Hostname -> Port -> Effect Unit
  , onRequestError :: Error -> Effect Unit
  }


defaultOptions :: Options
defaultOptions =
  { hostname: Hostname "0.0.0.0"
  , port: Port 3000
  , onListening: const (const (pure unit))
  , onRequestError: const (pure unit)
  }


defaultOptionsWithLogging :: Options
defaultOptionsWithLogging =
  defaultOptions { onListening = onListening
                 , onRequestError = onRequestError
                 }
  where
    onListening (Hostname hostname) (Port port) =
      log ("Listening on http://" <> hostname <> ":" <> show port)
    onRequestError err =
      log ("Request failed: " <> show err)

