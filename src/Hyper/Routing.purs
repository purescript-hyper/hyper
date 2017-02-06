-- Experimental implementation of servant-server style
-- routing for Hyper.
module Hyper.Routing
       ( Lit
       , Capture
       , CaptureAll
       , Handler
       , Raw
       , Sub
       , LitSub
       , AltE(..)
       , type (:>)
       , type (:/)
       , type (:<|>)
       , (:<|>)
       ) where

-- | A literal path segment, matching paths where the next segment is equal
-- | to the value of the `Symbol`.
-- |
-- | For example, the type `Lit "settings" :> Lit "general" :> "reset"`  would
-- | match the path `/settings/general/reset`.
data Lit (v :: Symbol)

-- | Captures one segment of a path as type `t`. The `v` is a
-- | `Symbol` that describes the captured value.
data Capture (v :: Symbol) t

-- | Captures all remaining segments of a path, all as type `t`. The `v`
-- | is a `Symbol` that describes
data CaptureAll (v :: Symbol) t

-- | A type-level description of the handler function, terminating a chain of
-- | path literals, captures, and other endpoint type constructs. The `m` symbol
-- | is the HTTP method that is handled. `ct` is the content type.
data Handler (m :: Symbol) ct b

-- | A type-level description of a raw handler middleware, terminating a chain
-- | of path literals, captures, and other endpoint type constructs. The `m`
-- | symbol is the HTTP method that is handled.
data Raw (m :: Symbol)

-- | The `Sub` is used to create the chain of `Lit`, `Capture`, `Handler`,
-- | and other such type constructs that build up an endpoint type. `Sub`
-- | is more often used infix with the `:>` operator.
data Sub e t

-- | A handy type alias for `Sub (Lit v)`, meant to be used infix with the `:/`
-- | operator. Instead of writing `Lit "a" :> Lit "b" :> ...`, you can write
-- | `"a" :/ "b" :/ ...`.
type LitSub (v :: Symbol) t = Sub (Lit v) t

-- | `AltE` respresents choice, i.e. that endpoint `a` is tried first, and if
-- | it fails, `b` is tried next. `AltE` is written infix using `:<|>` and is
-- | used to compose multiple endpoint types into a larger API or site. It is
-- | used to build up recursive structures, so `AltE a (AltE b c)` can be
-- | written `a :<|> b :<|> c`.
-- |
-- | It it also used to extract information from a type, where the information
-- | has the same structure as the type. For instance, when extracting links
-- | from an `AltE` type, you can pattern match the result using `:<|>`
-- | to access the links of `a` and `b`. That also works recursively with a
-- | pattern match like `a :<|> b :<|> c :<|> d`.
data AltE a b = AltE a b

infixr 5 type Sub as :>
infixr 5 type LitSub as :/
infixl 4 type AltE as :<|>
infixl 4 AltE as :<|>
