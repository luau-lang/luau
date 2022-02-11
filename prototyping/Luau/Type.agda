module Luau.Type where

open import FFI.Data.Maybe using (Maybe; just; nothing)

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

optional : Type → Type
optional nil = nil
optional (T ∪ nil) = (T ∪ nil)
optional T = (T ∪ nil)

normalizeOptional : Type → Type
normalizeOptional (S ∪ T) with normalizeOptional S | normalizeOptional T
normalizeOptional (S ∪ T) | (S′ ∪ nil) | (T′ ∪ nil) = (S′ ∪ T′) ∪ nil
normalizeOptional (S ∪ T) | S′         | (T′ ∪ nil) = (S′ ∪ T′) ∪ nil
normalizeOptional (S ∪ T) | (S′ ∪ nil) | T′         = (S′ ∪ T′) ∪ nil
normalizeOptional (S ∪ T) | S′         | nil        = optional S′
normalizeOptional (S ∪ T) | nil        | T′         = optional T′
normalizeOptional (S ∪ T) | S′         | T′         = S′ ∪ T′
normalizeOptional T = T

