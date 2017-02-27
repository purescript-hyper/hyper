module Hyper.Routing.Form where

import Prelude
import Data.String as String
import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Hyper.Routing (type (:<|>), type (:>), Capture, CaptureAll, Handler, Lit, (:<|>))
import Hyper.Routing.PathPiece (class ToPathPiece, toPathPiece)
import Text.Smolder.HTML (form, input)
import Text.Smolder.HTML.Attributes (action, method, name, type', value)
import Text.Smolder.Markup (MarkupM, (!))
import Type.Proxy (Proxy(..))

data FAppend a b = FAppend a b

infixl 4 type FAppend as :<>
infixl 4 FAppend as :<>

data InputText (name ∷ Symbol) = InputText String
data InputHidden (name ∷ Symbol) a = InputHidden a
data InputNumber (name ∷ Symbol) = InputNumber Int

class ToFormFields f o | f → o where
  toFields ∷ Proxy f → o

instance toFormFieldsInputText ∷ IsSymbol name
                                 ⇒ ToFormFields (InputText name) (Maybe String → MarkupM Unit Unit) where
  toFields _ s = maybe field (\x → field ! value x) s
    where
      n = reflectSymbol (SProxy ∷ SProxy name)
      field = input ! name n ! type' "text"

instance toFormFieldsInputNumber ∷ IsSymbol name
                                   ⇒ ToFormFields (InputNumber name) (Maybe Int → MarkupM Unit Unit) where
  toFields _ x = maybe field (\x' → field ! value (show x')) x
    where
      n = reflectSymbol (SProxy ∷ SProxy name)
      field = input ! name n ! type' "number"

instance toFormFieldsInputHidden ∷ (IsSymbol name, Show a)
                                   ⇒ ToFormFields (InputHidden name a) (a → MarkupM Unit Unit) where
  toFields _ x = input ! name n ! type' "hidden" ! value (show x)
    where
      n = reflectSymbol (SProxy ∷ SProxy name)

instance toFormFieldsFConcat ∷ (ToFormFields f1 m1, ToFormFields f2 m2)
                               ⇒ ToFormFields (f1 :<> f2) (m1 :<> m2) where
  toFields _ = toFields p1 :<> toFields p2
    where
      p1 = Proxy ∷ Proxy f1
      p2 = Proxy ∷ Proxy f2

data FormContext
  = FormContext (Array String)

class ToForm e r | e -> r where
  toForm :: Proxy e -> FormContext -> r

instance toFormAltE :: (ToForm e1 r1, ToForm e2 r2)
                       => ToForm (e1 :<|> e2) (r1 :<|> r2) where
  toForm _ ctx =
    toForm p1 ctx :<|> toForm p2 ctx
    where
      p1 = Proxy ∷ Proxy e1
      p2 = Proxy ∷ Proxy e2

instance toFormLit :: (ToForm sub r, IsSymbol lit)
                      => ToForm (Lit lit :> sub) r where
  toForm _ (FormContext segments) =
    toForm (Proxy ∷ Proxy sub) ctx
    where
      ctx = FormContext (segments <> [segment])
      segment = reflectSymbol (SProxy ∷ SProxy lit)

instance toFormCapture :: (ToForm sub r, IsSymbol c, ToPathPiece t)
                          => ToForm (Capture c t :> sub) (t → r) where
  toForm _ (FormContext segments) x =
    toForm (Proxy ∷ Proxy sub) ctx
    where
      ctx = FormContext (segments <> [toPathPiece x])

instance toFormCaptureAll :: (ToForm sub r, IsSymbol c, ToPathPiece t)
                             => ToForm (CaptureAll c t :> sub) (Array t → r) where
  toForm _ (FormContext segments) x =
    toForm (Proxy ∷ Proxy sub) ctx
    where
      ctx = FormContext (segments <> (map toPathPiece x))

-- TODO: Consider supporting GET with forms and query params.
instance toFormHandlerGet :: ToForm
                             (Handler "GET" ct b)
                             Unit where
  toForm _ (FormContext segments) = unit

instance toFormHandlerPost :: ToForm
                              (Handler "POST" ct b)
                              (MarkupM Unit Unit) where
  toForm _ (FormContext segments) =
    form ! method "POST" ! action path $ pure unit
    where
      path = "/" <> String.joinWith "/" segments

toForms
  ∷ ∀ e m. ToForm e m
    ⇒ Proxy e
    → m
toForms p = toForm p (FormContext [])
