module Luau.Type where

data Type : Set where
  nil : Type
  _⇒_ : Type → Type → Type
  none : Type
  any : Type
  _∪_ : Type → Type → Type
  _∩_ : Type → Type → Type

src : Type → Type
src nil = none
src (S ⇒ T) = S
src none = none
src any = any
src (S ∪ T) = (src S) ∪ (src T)
src (S ∩ T) = (src S) ∩ (src T)

tgt : Type → Type
tgt nil = none
tgt (S ⇒ T) = T
tgt none = none
tgt any = any
tgt (S ∪ T) = (tgt S) ∪ (tgt T)
tgt (S ∩ T) = (tgt S) ∩ (tgt T)
