module Examples.FormParser where

import Prelude

import Control.Bind.Indexed (ibind)
import Control.Monad.Indexed ((:>>=), (:*>))
import Data.Either (Either(Right, Left))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.MediaType.Common (textHTML)
import Data.String (length)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Hyper.Conn (BodyRead, BodyUnread, Conn, ResponseEnded, StatusLineOpen)
import Hyper.Form (parseForm, required)
import Hyper.Middleware (Middleware)
import Hyper.Node.Server (HttpRequest, HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Request (getRequestData, ignoreBody)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Status (Status, statusBadRequest, statusMethodNotAllowed, statusOK)
import Text.Smolder.HTML (button, form, input, label, p)
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.String (render)

main :: Effect Unit
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

    htmlWithStatus :: forall reqState comp
                    . Status
                   -> Markup _
                   -> Middleware
                        Aff
                        (Conn HttpRequest reqState HttpResponse StatusLineOpen comp)
                        (Conn HttpRequest reqState HttpResponse ResponseEnded comp)
                        Unit
    htmlWithStatus status x =
      writeStatus status
      :*> contentType textHTML
      :*> closeHeaders
      :*> respond (render x)

    handlePost :: forall comp
                . Middleware
                   Aff
                   (Conn HttpRequest BodyUnread HttpResponse StatusLineOpen comp)
                   (Conn HttpRequest BodyRead HttpResponse ResponseEnded comp)
                   Unit
    handlePost =
      parseForm :>>=
      case _ of
        Left err -> do
          liftEffect (log err)
          :*> htmlWithStatus statusBadRequest
                (p (text "Bad request, invalid form."))
        Right form ->
          case required "firstName" form of
            Right name
              | length name > 0 ->
                htmlWithStatus statusOK (p (text ("Hi " <> name <> "!")))
              | otherwise ->
                htmlWithStatus statusBadRequest
                  (renderNameForm (Just "Name cannot be empty."))
            Left err ->
              htmlWithStatus statusBadRequest (renderNameForm (Just err))

    -- Our (rather primitive) router.
    router :: forall comp
                . Middleware
                   Aff
                   (Conn HttpRequest BodyUnread HttpResponse StatusLineOpen comp)
                   (Conn HttpRequest BodyRead HttpResponse ResponseEnded comp)
                   Unit
    router = let bind = ibind in do
      reqData <- getRequestData
      case reqData.method of
        Left GET -> do
          _ <- ignoreBody
          htmlWithStatus statusOK (renderNameForm Nothing)
        Left POST -> do
          handlePost
        method -> do
          _ <- ignoreBody
          htmlWithStatus statusMethodNotAllowed
            (text ("Method not supported: " <> show method))

  -- Let's run it.
  in runServer defaultOptionsWithLogging {} router
