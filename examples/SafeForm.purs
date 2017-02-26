module Examples.SafeForm where

import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (ExceptT)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Hyper.Form.Safe (type (:<>), InputHidden, InputNumber, InputText, toForm, (:<>))
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture, (:<|>))
import Hyper.Routing.ContentType.HTML (class EncodeHTML, HTML, linkTo)
import Hyper.Routing.Links (linksTo)
import Hyper.Routing.Method (Get)
import Hyper.Routing.Router (RoutingError, router)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (h1, label, p, table, tbody, td, th, thead, tr)
import Text.Smolder.HTML.Attributes (placeholder)
import Text.Smolder.Markup (text, (!))
import Type.Proxy (Proxy(..))
import Prelude hiding (div)

type Site =
  Get HTML Persons
  :<|> "new" :/ Get HTML NewPerson
  :<|> Capture "id" Int :> Get HTML EditPerson

newtype Person = Person { id :: Int, name ∷ String, age ∷ Int }

data Persons = Persons (Array Person)
data NewPerson = NewPerson
data EditPerson = EditPerson Person

type PersonForm =
  InputHidden "id" Int
  :<> InputText "name"
  :<> InputNumber "age"

instance encodeHTMLPersons :: EncodeHTML Persons where
  encodeHTML (Persons ps) =
    table do
      thead do
        tr do
          th (text "Name")
          th (text "Age")
          th (text "Actions")
      tbody (traverse_ encodePerson ps)
    where
      encodePerson (Person person) =
        case linksTo site of
          _ :<|> _ :<|> getPerson' →
            tr do
              td (text person.name)
              td (text (show person.age))
              td (linkTo (getPerson' person.id) (text "Edit"))

instance encodeHTMLNewPerson :: EncodeHTML NewPerson where
  encodeHTML _ =
    case toForm (Proxy :: Proxy PersonForm) of
      idField :<> nameField :<> ageField → do
        idField 0
        label do
          text "Name: "
          nameField Nothing ! placeholder "Jane Doe..."
        label do
          text "Age: "
          ageField Nothing

instance encodeHTMLPerson :: EncodeHTML EditPerson where
  encodeHTML (EditPerson (Person person)) = do
    h1 (text "Edit Person")
    p (text "TODO")

allPersons ∷ ∀ m. Monad m ⇒ ExceptT RoutingError m Persons
allPersons =
  [Person { id: 1, name: "Alice", age: 41 }]
  # Persons
  # pure

newPerson ∷ ∀ m. Monad m ⇒ ExceptT RoutingError m NewPerson
newPerson = pure NewPerson

editPerson ∷ ∀ m. Monad m ⇒ Int → ExceptT RoutingError m EditPerson
editPerson i =
  Person { id: 0, name: "John", age: 41 }
  # EditPerson
  # pure

site :: Proxy Site
site = Proxy

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, buffer :: BUFFER | e) Unit
main =
  let onRoutingError status msg =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" id msg)
      appRouter = router site (allPersons :<|> newPerson :<|> editPerson) onRoutingError
  in runServer defaultOptions {} appRouter
