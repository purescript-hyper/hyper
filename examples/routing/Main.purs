module Main where

import Prelude
import Control.Alternative ((<|>))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.MediaType.Common (textHTML)
import Hyper.Core (HeadersOpen, class ResponseWriter, ResponseEnded, Conn, Middleware, closeHeaders, Port(Port))
import Hyper.HTML.DSL (linkTo, h1, p, text, html)
import Hyper.Method (Method)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (contentType, notFound)
import Hyper.Router (notSupported, resource, fallbackTo, handler)
import Node.HTTP (HTTP)

app :: forall m req res rw c.
       (Monad m, ResponseWriter rw m) =>
       Middleware
       m
       (Conn { url :: String, method :: Method | req }
             { writer :: rw HeadersOpen | res }
             c)
       (Conn { url :: String, method :: Method | req }
             { writer :: rw ResponseEnded | res }
             c)
app =
  fallbackTo
  -- Not Found:
  (contentType textHTML >=> closeHeaders >=> notFound)
  -- Resources:
  (resource home <|> resource about)
    where
      homeView =
        html do
          h1 [] (text "Welcome!")
          p [] do
            text "Read more at "
            -- Type-safe routing:
            linkTo about (text "About")
            text "."

      home = { path: []
             , "GET":
               handler (contentType textHTML
                        >=> closeHeaders
                        >=> homeView)
             , "POST": notSupported
             }

      aboutView = html do
        h1 [] (text "About")
        p [] (text "OK, about this example...")

      about = { path: ["about"]
              , "GET": handler (contentType textHTML
                                >=> closeHeaders
                                >=> aboutView)
              , "POST": notSupported
              }


main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR | e) Unit
main =
  let
    -- Some nice console printing when the server starts, and if a request
    -- fails (in this case when the request body is unreadable for some reason).
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

  -- Let's run it.
  in runServer defaultOptions onListening onRequestError app
