module Luau.Type where

open import FFI.Data.Maybe using (Maybe; just; nothing)

data Type : Set where
  nil : Type
  _⇒_ : Type → Type → Type
  bot : Type
  top : Type
  _∪_ : Type → Type → Type
  _∩_ : Type → Type → Type

data Mode : Set where
  strict : Mode
  nonstrict : Mode

src : Mode → Type → Type
src m nil = bot
src m (S ⇒ T) = S
-- In nonstrict mode, functions are covaraiant, in strict mode they're contravariant
src strict    (S ∪ T) = (src strict S) ∩ (src strict T)
src nonstrict (S ∪ T) = (src nonstrict S) ∪ (src nonstrict T)
src strict    (S ∩ T) = (src strict S) ∪ (src strict T)
src nonstrict (S ∩ T) = (src nonstrict S) ∩ (src nonstrict T)
src strict bot = top
src nonstrict bot = bot
src strict top = bot
src nonstrict top = top

tgt : Type → Type
tgt nil = bot
tgt (S ⇒ T) = T
tgt bot = bot
tgt top = top
tgt (S ∪ T) = (tgt S) ∪ (tgt T)
tgt (S ∩ T) = (tgt S) ∩ (tgt T)

optional : Type → Type
optional nil = nil
optional (T ∪ nil) = (T ∪ nil)
optional T = (T ∪ nil)

normalizeOptional : Type → Type
normalizeOptional (S ∪ T) with normalizeOptional S | normalizeOptional T
normalizeOptional (S ∪ T) | (S′ ∪ nil) | (T′ ∪ nil) = (S′ ∪ T′) ∪ nil
normalizeOptional (S ∪ T) | S′ | nil = optional S′
normalizeOptional (S ∪ T) | nil | T′ = optional T′
normalizeOptional (S ∪ T) | S′ | T′ = S′ ∪ T′
normalizeOptional T = T

