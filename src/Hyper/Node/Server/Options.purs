module Hyper.Node.Server.Options
       ( defaultOptions
       , defaultOptionsWithLogging
       , Hostname(..)
       , Options(..)
       , Port(..)
       ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Data.Newtype (class Newtype)
import Node.HTTP (HTTP)
import Prelude

newtype Hostname = Hostname String
derive instance newtypeHostname :: Newtype Hostname _

newtype Port = Port Int
derive instance newtypePort :: Newtype Port _

type Options e =
  { hostname :: Hostname
  , port :: Port
  , onListening :: Hostname -> Port -> Eff (http :: HTTP | e) Unit
  , onRequestError :: Error -> Eff (http :: HTTP | e) Unit
  }


defaultOptions :: forall e. Options e
defaultOptions =
  { hostname: Hostname "0.0.0.0"
  , port: Port 3000
  , onListening: const (const (pure unit))
  , onRequestError: const (pure unit)
  }


defaultOptionsWithLogging :: forall e. Options (console :: CONSOLE | e)
defaultOptionsWithLogging =
  defaultOptions { onListening = onListening
                 , onRequestError = onRequestError
                 }
  where
    onListening (Hostname hostname) (Port port) =
      log ("Listening on http://" <> hostname <> ":" <> show port)
    onRequestError err =
      log ("Request failed: " <> show err)

