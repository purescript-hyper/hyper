module Examples.FormParser where

import Prelude
import Text.Smolder.HTML.Attributes as A
import Control.IxMonad ((:>>=), (:*>))
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
import Data.Tuple (lookup)
import Hyper.Form (Form(Form), parseForm)
import Hyper.Middleware.Class (getConn)
import Hyper.Node.Server (readBodyAsString, defaultOptions, runServer)
import Hyper.Port (Port(..))
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed, statusOK)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (button, form, input, label, p)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.String (render)

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, err :: EXCEPTION, avar :: AVAR | e) Unit
main =
  let
    -- A view function that renders the name form.
    renderNameForm err = do
      form ! A.method "post" $ do
        formattedError
        formElements
      where
        formElements = do
          label ! A.for "firstName" $ text "Your Name:"
          p (input ! A.name "firstName" ! A.id "firstName")
          p (button (text "Send"))

        formattedError =
          case err of
            Just s -> p ! A.style "color: red;" $ text s
            Nothing -> pure unit

    htmlWithStatus status x =
      writeStatus status
      :*> contentType textHTML
      :*> closeHeaders
      :*> respond (render x)


    handlePost =
      parseForm :>>=
      case _ of
        Left err -> do
          liftEff (log (message err))
          :*> htmlWithStatus
              statusBadRequest
              (p (text "Bad request, invalid form."))
        Right (Form values) ->
          case lookup "firstName" values of
            Just name | length name > 0 ->
              htmlWithStatus
              statusOK
              (p (text ("Hi " <> name <> "!")))
            _ ->
              htmlWithStatus
              statusBadRequest
              (renderNameForm (Just "Name is missing."))

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
    app = readBodyAsString :*> router

    -- Some nice console printing when the server starts, and if a request
    -- fails (in this case when the request body is unreadable for some reason).
    onListening (Port port) = log ("Listening on http://localhost:" <> show port)
    onRequestError err = log ("Request failed: " <> show err)

  -- Let's run it.
  in runServer defaultOptions onListening onRequestError {} app
