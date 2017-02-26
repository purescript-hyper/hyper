module Hyper.Form.Safe where

import Prelude
import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Text.Smolder.HTML (input)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup (MarkupM, (!))
import Type.Proxy (Proxy(..))

--

data FAppend a b = FAppend a b

infixl 4 type FAppend as :<>
infixl 4 FAppend as :<>

--

data InputText (name ∷ Symbol) = InputText String
data InputHidden (name ∷ Symbol) a = InputHidden a
data InputNumber (name ∷ Symbol) = InputNumber Int

--

class ToFormHTML f o | f → o where
  toForm ∷ Proxy f → o

instance toFormHtmlInputText ∷ IsSymbol name
                               ⇒ ToFormHTML (InputText name) (Maybe String → MarkupM Unit Unit) where
  toForm _ s = maybe field (\x → field ! value x) s
    where
      n = reflectSymbol (SProxy ∷ SProxy name)
      field = input ! name n ! type' "text"

instance toFormHtmlInputNumber ∷ IsSymbol name
                                 ⇒ ToFormHTML (InputNumber name) (Maybe Int → MarkupM Unit Unit) where
  toForm _ x = maybe field (\x' → field ! value (show x')) x
    where
      n = reflectSymbol (SProxy ∷ SProxy name)
      field = input ! name n ! type' "number"

instance toFormHtmlInputHidden ∷ (IsSymbol name, Show a)
                                 ⇒ ToFormHTML (InputHidden name a) (a → MarkupM Unit Unit) where
  toForm _ x = input ! name n ! type' "hidden" ! value (show x)
    where
      n = reflectSymbol (SProxy ∷ SProxy name)

instance toFormHtmlFConcat ∷ (ToFormHTML f1 m1, ToFormHTML f2 m2)
                             ⇒ ToFormHTML (f1 :<> f2) (m1 :<> m2) where
  toForm _ = toForm p1 :<> toForm p2
    where
      p1 = Proxy ∷ Proxy f1
      p2 = Proxy ∷ Proxy f2
