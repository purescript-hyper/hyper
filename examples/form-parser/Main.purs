module Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (message, EXCEPTION)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Nothing, Just))
import Data.MediaType.Common (textHTML)
import Data.String (length)
import Data.Tuple (lookup, Tuple(Tuple))
import Hyper.Core (statusOK, statusBadRequest, writeStatus, closeHeaders, Port(Port))
import Hyper.Form (Form(Form), parseForm)
import Hyper.HTML.DSL (p, text, element, html)
import Hyper.Method (Method(POST, GET))
import Hyper.Node.Server (readBodyAsString, defaultOptions, runServer)
import Hyper.Response (contentType)
import Node.HTTP (HTTP)

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR | e) Unit
main =
  let
    -- A view function that renders the name form.
    renderNameForm err = do
      errHtml
      element "form" [(Tuple "method" "post")] do
        element "label" [Tuple "for" "firstName"] do
          text "Your Name:"
        p [] $
          element "input" [ Tuple "name" "firstName"
                          , Tuple "id" "firstName"
                          ] (pure unit)
        p [] $
          element "button" [] (text "Send")
      where errHtml =
              case err of
                Just s -> p [(Tuple "style" "color: red;")] (text s)
                Nothing -> pure unit

    htmlWithStatus status x =
      writeStatus status
      >=> contentType textHTML
      >=> closeHeaders
      >=> html x

    handlePost body conn =
      case body of
        Left err -> do
          liftEff (log (message err))
          htmlWithStatus
            statusBadRequest
            (p [] (text "Bad request, invalid form."))
            conn
        Right (Form values) ->
          case lookup "firstName" values of
            Just name | length name > 0 ->
              htmlWithStatus
              statusOK
              (p [] (text ("Hi " <> name <> "!")))
              conn
            _ ->
              htmlWithStatus
              statusBadRequest
              (renderNameForm (Just "Name is missing."))
              conn

    -- Our handler for GET and POST requests.
    handler conn =
      case conn.request.method of
        GET -> htmlWithStatus
               statusOK
               (renderNameForm Nothing)
               conn
        POST -> handlePost conn.request.body conn

    -- A chain of middleware for parsing the form, and then our response
    -- handler.
    app = readBodyAsString
          >=> parseForm
          >=> handler

    -- Some nice console printing when the server starts, and if a request
    -- fails (in this case when the request body is unreadable for some reason).
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

  -- Let's run it.
  in runServer defaultOptions onListening onRequestError app
