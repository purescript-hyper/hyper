module Main where

import Prelude
import Control.Alternative ((<|>))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.MediaType.Common (textHTML)
import Hyper.Core (closeHeaders, writeStatus, StatusLineOpen, class ResponseWriter, ResponseEnded, Conn, Middleware, Port(Port))
import Hyper.HTML (asString, element_, h1, p, text)
import Hyper.Method (Method)
import Hyper.Node.Server (ResponseBody, defaultOptions, runServer)
import Hyper.Response (class Response, respond, contentType)
import Hyper.Routing.ResourceRouter (defaultRouterFallbacks, router, linkTo, resource, runRouter, handler)
import Hyper.Status (statusOK)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)

app :: forall m req res rw c.
       (Monad m, ResponseWriter rw m ResponseBody, Response ResponseBody m String) =>
       Middleware
       m
       (Conn { url :: String, method :: Method | req }
             { writer :: rw StatusLineOpen | res }
             c)
       (Conn { url :: String, method :: Method | req }
             { writer :: rw ResponseEnded | res }
             c)
app =
  runRouter
  defaultRouterFallbacks

  -- Resources:
  (router home <|> router about)
    where
      htmlWithStatus status doc =
        writeStatus status
        >=> contentType textHTML
        >=> closeHeaders
        >=> respond (asString doc)

      homeView =
        element_ "section" [ h1 [] [ text "Welcome!" ]
                           , p [] [ text "Read more at "
                                    -- Type-safe routing:
                                  , linkTo about [text "About"]
                                  , text "."
                                  ]
                           ]

      home =
        resource { path = []
                 , "GET" = handler (htmlWithStatus statusOK homeView)
                 }

      aboutView =
        element_ "section" [ h1 [] [ text "About" ]
                           , p [] [ text "OK, about this example..." ]
                           ]

      about =
        resource { path = ["about"]
                 , "GET" = handler (htmlWithStatus statusOK aboutView)
                 }


main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  let
    -- Some nice console printing when the server starts, and if a request
    -- fails (in this case when the request body is unreadable for some reason).
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

  -- Let's run it.
  in runServer defaultOptions onListening onRequestError {} app
