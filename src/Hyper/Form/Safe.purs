module Hyper.Form.Safe where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Text.Smolder.HTML (input, label)
import Text.Smolder.HTML.Attributes (name, placeholder, type')
import Text.Smolder.Markup (MarkupM, text, (!))
import Text.Smolder.Renderer.String (render)
import Type.Proxy (Proxy(..))

--

data FAppend a b = FAppend a b

infixl 4 type FAppend as :<>
infixl 4 FAppend as :<>

--

data InputText (name ∷ Symbol) = InputText
data InputHidden (name ∷ Symbol) = InputHidden
data InputNumber (name ∷ Symbol) = InputNumber

--

class ToFormHTML f o | f → o where
  toForm ∷ Proxy f → o

instance toFormHtmlInputText ∷ IsSymbol name ⇒ ToFormHTML (InputText name) (MarkupM Unit Unit) where
  toForm _ = input ! name n ! type' "text"
    where n = reflectSymbol (SProxy ∷ SProxy name)

instance toFormHtmlInputNumber ∷ IsSymbol name ⇒ ToFormHTML (InputNumber name) (MarkupM Unit Unit) where
  toForm _ = input ! name n ! type' "number"
    where n = reflectSymbol (SProxy ∷ SProxy name)

instance toFormHtmlInputHidden ∷ IsSymbol name ⇒ ToFormHTML (InputHidden name) (MarkupM Unit Unit) where
  toForm _ = input ! name n ! type' "hidden"
    where n = reflectSymbol (SProxy ∷ SProxy name)

instance toFormHtmlFConcat ∷ (ToFormHTML f1 m1, ToFormHTML f2 m2)
                             ⇒ ToFormHTML (f1 :<> f2) (m1 :<> m2) where
  toForm _ = toForm p1 :<> toForm p2
    where
      p1 = Proxy ∷ Proxy f1
      p2 = Proxy ∷ Proxy f2

--

type PersonForm =
  InputText "id"
  :<> InputText "name"
  :<> InputNumber "age"

test ∷ String
test =
  render $
  case toForm (Proxy :: Proxy PersonForm) of
    idField :<> nameField :<> ageField → do
      idField
      label do
        text "Name: "
        nameField ! placeholder "Jane Doe…"
      label do
        text "Age: "
        ageField
