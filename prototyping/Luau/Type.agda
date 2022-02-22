module Luau.Type where

open import FFI.Data.Maybe using (Maybe; just; nothing; just-inv)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Equality using (cong)

data Type : Set where
  nil : Type
  _⇒_ : Type → Type → Type
  bot : Type
  top : Type
  _∪_ : Type → Type → Type
  _∩_ : Type → Type → Type

lhs : Type → Type
lhs (T ⇒ _) = T
lhs (T ∪ _) = T
lhs (T ∩ _) = T
lhs nil = nil
lhs bot = bot
lhs top = top

rhs : Type → Type
rhs (_ ⇒ T) = T
rhs (_ ∪ T) = T
rhs (_ ∩ T) = T
rhs nil = nil
rhs bot = bot
rhs top = top

_≡ᵀ_ : ∀ (T U : Type) → Dec(T ≡ U)
nil ≡ᵀ nil = yes refl
nil ≡ᵀ (S ⇒ T) = no (λ ())
nil ≡ᵀ bot = no (λ ())
nil ≡ᵀ top = no (λ ())
nil ≡ᵀ (S ∪ T) = no (λ ())
nil ≡ᵀ (S ∩ T) = no (λ ())
(S ⇒ T) ≡ᵀ nil = no (λ ())
(S ⇒ T) ≡ᵀ (U ⇒ V) with (S ≡ᵀ U) | (T ≡ᵀ V) 
(S ⇒ T) ≡ᵀ (S ⇒ T) | yes refl | yes refl = yes refl
(S ⇒ T) ≡ᵀ (U ⇒ V) | _ | no p = no (λ q → p (cong rhs q))
(S ⇒ T) ≡ᵀ (U ⇒ V) | no p | _ = no (λ q → p (cong lhs q))
(S ⇒ T) ≡ᵀ bot = no (λ ())
(S ⇒ T) ≡ᵀ top = no (λ ())
(S ⇒ T) ≡ᵀ (U ∪ V) = no (λ ())
(S ⇒ T) ≡ᵀ (U ∩ V) = no (λ ())
bot ≡ᵀ nil = no (λ ())
bot ≡ᵀ (U ⇒ V) = no (λ ())
bot ≡ᵀ bot = yes refl
bot ≡ᵀ top = no (λ ())
bot ≡ᵀ (U ∪ V) = no (λ ())
bot ≡ᵀ (U ∩ V) = no (λ ())
top ≡ᵀ nil = no (λ ())
top ≡ᵀ (U ⇒ V) = no (λ ())
top ≡ᵀ bot = no (λ ())
top ≡ᵀ top = yes refl
top ≡ᵀ (U ∪ V) = no (λ ())
top ≡ᵀ (U ∩ V) = no (λ ())
(S ∪ T) ≡ᵀ nil = no (λ ())
(S ∪ T) ≡ᵀ (U ⇒ V) = no (λ ())
(S ∪ T) ≡ᵀ bot = no (λ ())
(S ∪ T) ≡ᵀ top = no (λ ())
(S ∪ T) ≡ᵀ (U ∪ V) with (S ≡ᵀ U) | (T ≡ᵀ V) 
(S ∪ T) ≡ᵀ (S ∪ T) | yes refl | yes refl = yes refl
(S ∪ T) ≡ᵀ (U ∪ V) | _ | no p = no (λ q → p (cong rhs q))
(S ∪ T) ≡ᵀ (U ∪ V) | no p | _ = no (λ q → p (cong lhs q))
(S ∪ T) ≡ᵀ (U ∩ V) = no (λ ())
(S ∩ T) ≡ᵀ nil = no (λ ())
(S ∩ T) ≡ᵀ (U ⇒ V) = no (λ ())
(S ∩ T) ≡ᵀ bot = no (λ ())
(S ∩ T) ≡ᵀ top = no (λ ())
(S ∩ T) ≡ᵀ (U ∪ V) = no (λ ())
(S ∩ T) ≡ᵀ (U ∩ V) with (S ≡ᵀ U) | (T ≡ᵀ V) 
(S ∩ T) ≡ᵀ (U ∩ V) | yes refl | yes refl = yes refl
(S ∩ T) ≡ᵀ (U ∩ V) | _ | no p = no (λ q → p (cong rhs q))
(S ∩ T) ≡ᵀ (U ∩ V) | no p | _ = no (λ q → p (cong lhs q))

_≡ᴹᵀ_ : ∀ (T U : Maybe Type) → Dec(T ≡ U)
nothing ≡ᴹᵀ nothing = yes refl
nothing ≡ᴹᵀ just U = no (λ ())
just T ≡ᴹᵀ nothing = no (λ ())
just T ≡ᴹᵀ just U with T ≡ᵀ U
(just T ≡ᴹᵀ just T) | yes refl = yes refl
(just T ≡ᴹᵀ just U) | no p = no (λ q → p (just-inv q))

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

