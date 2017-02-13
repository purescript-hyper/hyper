module Main where

import Prelude
import Control.IxMonad (ibind, (:>>=))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (message, EXCEPTION)
import Data.Either (Either(Right, Left))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.MediaType.Common (textHTML)
import Data.String (length)
import Data.Tuple (lookup, Tuple(Tuple))
import Hyper.Form (Form(Form), parseForm)
import Hyper.HTML (asString, element, p, text)
import Hyper.Middleware.Class (getConn)
import Hyper.Node.Server (readBodyAsString, defaultOptions, runServer)
import Hyper.Port (Port(..))
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed, statusOK)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  let
    -- A view function that renders the name form.
    renderNameForm err = do
      element "form" [(Tuple "method" "post")] (join [ formattedError, formElements ])
      where
        formElements =
          [ element "label" [ Tuple "for" "firstName" ] [ text "Your Name:" ]
          , p [] [ element "input" [ Tuple "name" "firstName"
                                   , Tuple "id" "firstName"
                                   ] []
                 ]
          , p [] [ element "button" [] [ text "Send" ] ]
          ]

        formattedError =
          case err of
            Just s -> [ p [ Tuple "style" "color: red;" ] [ text s ] ]
            Nothing -> []

    htmlWithStatus status x = do
      writeStatus status
      contentType textHTML
      closeHeaders
      respond (asString x)
      where bind = ibind

    handlePost = do
      body ← parseForm
      case body of
        Left err -> do
          liftEff (log (message err))
          htmlWithStatus
            statusBadRequest
            (p [] [text "Bad request, invalid form."])
        Right (Form values) ->
          case lookup "firstName" values of
            Just name | length name > 0 ->
              htmlWithStatus
              statusOK
              (p [] [text ("Hi " <> name <> "!")])
            _ ->
              htmlWithStatus
              statusBadRequest
              (renderNameForm (Just "Name is missing."))
      where bind = ibind

    -- Our (rather primitive) router.
    router =
      getConn :>>= \conn →
      case conn.request.method of
        Left GET ->
          htmlWithStatus
          statusOK
          (renderNameForm Nothing)
        Left POST ->
          handlePost
        method ->
          htmlWithStatus
          statusMethodNotAllowed
          (text ("Method not supported: " <> show method))

    -- A chain of middleware for parsing the form, and then our response
    -- handler.
    app = readBodyAsString
          :>>= const router

    -- Some nice console printing when the server starts, and if a request
    -- fails (in this case when the request body is unreadable for some reason).
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

  -- Let's run it.
  in runServer defaultOptions onListening onRequestError {} app
