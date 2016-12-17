module Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (message, EXCEPTION)
import Data.Either (Either(Right, Left))
import Data.Foldable (fold)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (length)
import Data.Tuple (lookup, Tuple(Tuple))
import Hyper.Core (Port(Port))
import Hyper.Form (Form(Form), parseForm)
import Hyper.Method (Method(POST, GET))
import Hyper.Node.Server (readBodyAsString, defaultOptions, runServer)
import Hyper.Response (respond, headers)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER, avar :: AVAR | e) Unit
main =
  let
    -- A view function that renders the name form.
    renderNameForm err conn =
      respond
      (fold [ errHtml
            , "<form method=\"post\">"
            , "<label>Your Name: <input method=\"post\" name=\"firstName\"></label>"
            , "<p><button type=\"submit\">Send</button></p>"
            , "</form>"
            ])
      conn
      where errHtml =
              case err of
                Just s -> "<p style=\"color: red;\">" <> s <> "</p>"
                Nothing -> ""

    -- Our handler for GET and POST requests.
    printForm conn =
      case conn.request.method of
        GET -> renderNameForm Nothing conn
        POST ->
          case conn.request.body of
            Left err -> do
              liftEff (log (message err))
              respond "Bad request, invalid form.\n" conn
            Right (Form values) ->
              case lookup "firstName" values of
                Just name | length name > 0 -> respond ("<p>Hi " <> name <> "!</p>") conn
                _ -> renderNameForm (Just "Name is missing.") conn

    -- Our handler always renders HTML.
    handler = headers [(Tuple "Content-Type" "text/html")] >=> printForm

    -- A chain of middleware for parsing the form, and then our response
    -- handler.
    app = readBodyAsString >=> parseForm >=> handler

    -- Some nice console printing when the server starts, and if a request
    -- fails (in this case when the request body is unreadable for some reason).
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

  -- Let's run it.
  in runServer defaultOptions onListening onRequestError app
