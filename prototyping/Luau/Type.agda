module Luau.Type where

open import FFI.Data.Maybe using (Maybe; just; nothing; just-inv)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Equality using (cong)
open import FFI.Data.Maybe using (Maybe; just; nothing)

data Type : Set where
  nil : Type
  _⇒_ : Type → Type → Type
  none : Type
  any : Type
  boolean : Type
  number : Type
  string : Type
  _∪_ : Type → Type → Type
  _∩_ : Type → Type → Type

lhs : Type → Type
lhs (T ⇒ _) = T
lhs (T ∪ _) = T
lhs (T ∩ _) = T
lhs nil = nil
lhs none = none
lhs any = any
lhs number = number
lhs boolean = boolean
lhs string = string

rhs : Type → Type
rhs (_ ⇒ T) = T
rhs (_ ∪ T) = T
rhs (_ ∩ T) = T
rhs nil = nil
rhs none = none
rhs any = any
rhs number = number
rhs boolean = boolean
rhs string = string

_≡ᵀ_ : ∀ (T U : Type) → Dec(T ≡ U)
nil ≡ᵀ nil = yes refl
nil ≡ᵀ (S ⇒ T) = no (λ ())
nil ≡ᵀ none = no (λ ())
nil ≡ᵀ any = no (λ ())
nil ≡ᵀ number = no (λ ())
nil ≡ᵀ boolean = no (λ ())
nil ≡ᵀ (S ∪ T) = no (λ ())
nil ≡ᵀ (S ∩ T) = no (λ ())
nil ≡ᵀ string = no (λ ())
(S ⇒ T) ≡ᵀ string = no (λ ())
none ≡ᵀ string = no (λ ())
any ≡ᵀ string = no (λ ())
boolean ≡ᵀ string = no (λ ())
number ≡ᵀ string = no (λ ())
(S ∪ T) ≡ᵀ string = no (λ ())
(S ∩ T) ≡ᵀ string = no (λ ())
(S ⇒ T) ≡ᵀ nil = no (λ ())
(S ⇒ T) ≡ᵀ (U ⇒ V) with (S ≡ᵀ U) | (T ≡ᵀ V) 
(S ⇒ T) ≡ᵀ (S ⇒ T) | yes refl | yes refl = yes refl
(S ⇒ T) ≡ᵀ (U ⇒ V) | _ | no p = no (λ q → p (cong rhs q))
(S ⇒ T) ≡ᵀ (U ⇒ V) | no p | _ = no (λ q → p (cong lhs q))
(S ⇒ T) ≡ᵀ none = no (λ ())
(S ⇒ T) ≡ᵀ any = no (λ ())
(S ⇒ T) ≡ᵀ number = no (λ ())
(S ⇒ T) ≡ᵀ boolean = no (λ ())
(S ⇒ T) ≡ᵀ (U ∪ V) = no (λ ())
(S ⇒ T) ≡ᵀ (U ∩ V) = no (λ ())
none ≡ᵀ nil = no (λ ())
none ≡ᵀ (U ⇒ V) = no (λ ())
none ≡ᵀ none = yes refl
none ≡ᵀ any = no (λ ())
none ≡ᵀ number = no (λ ())
none ≡ᵀ boolean = no (λ ())
none ≡ᵀ (U ∪ V) = no (λ ())
none ≡ᵀ (U ∩ V) = no (λ ())
any ≡ᵀ nil = no (λ ())
any ≡ᵀ (U ⇒ V) = no (λ ())
any ≡ᵀ none = no (λ ())
any ≡ᵀ any = yes refl
any ≡ᵀ number = no (λ ())
any ≡ᵀ boolean = no (λ ())
any ≡ᵀ (U ∪ V) = no (λ ())
any ≡ᵀ (U ∩ V) = no (λ ())
number ≡ᵀ nil = no (λ ())
number ≡ᵀ (T ⇒ U) = no (λ ())
number ≡ᵀ none = no (λ ())
number ≡ᵀ any = no (λ ())
number ≡ᵀ number = yes refl
number ≡ᵀ boolean = no (λ ())
number ≡ᵀ (T ∪ U) = no (λ ())
number ≡ᵀ (T ∩ U) = no (λ ())
boolean ≡ᵀ nil = no (λ ())
boolean ≡ᵀ (T ⇒ U) = no (λ ())
boolean ≡ᵀ none = no (λ ())
boolean ≡ᵀ any = no (λ ())
boolean ≡ᵀ boolean = yes refl
boolean ≡ᵀ number = no (λ ())
boolean ≡ᵀ (T ∪ U) = no (λ ())
boolean ≡ᵀ (T ∩ U) = no (λ ())
string ≡ᵀ nil = no (λ ())
string ≡ᵀ (x ⇒ x₁) = no (λ ())
string ≡ᵀ none = no (λ ())
string ≡ᵀ any = no (λ ())
string ≡ᵀ boolean = no (λ ())
string ≡ᵀ number = no (λ ())
string ≡ᵀ string = yes refl
string ≡ᵀ (U ∪ V) = no (λ ())
string ≡ᵀ (U ∩ V) = no (λ ())
(S ∪ T) ≡ᵀ nil = no (λ ())
(S ∪ T) ≡ᵀ (U ⇒ V) = no (λ ())
(S ∪ T) ≡ᵀ none = no (λ ())
(S ∪ T) ≡ᵀ any = no (λ ())
(S ∪ T) ≡ᵀ number = no (λ ())
(S ∪ T) ≡ᵀ boolean = no (λ ())
(S ∪ T) ≡ᵀ (U ∪ V) with (S ≡ᵀ U) | (T ≡ᵀ V) 
(S ∪ T) ≡ᵀ (S ∪ T) | yes refl | yes refl = yes refl
(S ∪ T) ≡ᵀ (U ∪ V) | _ | no p = no (λ q → p (cong rhs q))
(S ∪ T) ≡ᵀ (U ∪ V) | no p | _ = no (λ q → p (cong lhs q))
(S ∪ T) ≡ᵀ (U ∩ V) = no (λ ())
(S ∩ T) ≡ᵀ nil = no (λ ())
(S ∩ T) ≡ᵀ (U ⇒ V) = no (λ ())
(S ∩ T) ≡ᵀ none = no (λ ())
(S ∩ T) ≡ᵀ any = no (λ ())
(S ∩ T) ≡ᵀ number = no (λ ())
(S ∩ T) ≡ᵀ boolean = no (λ ())
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
src m nil = none
src m number = none
src m boolean = none
src m string = none
src m (S ⇒ T) = S
-- In nonstrict mode, functions are covaraiant, in strict mode they're contravariant
src strict    (S ∪ T) = (src strict S) ∩ (src strict T)
src nonstrict (S ∪ T) = (src nonstrict S) ∪ (src nonstrict T)
src strict    (S ∩ T) = (src strict S) ∪ (src strict T)
src nonstrict (S ∩ T) = (src nonstrict S) ∩ (src nonstrict T)
src strict none = any
src nonstrict none = none
src strict any = none
src nonstrict any = any

tgt : Type → Type
tgt nil = none
tgt (S ⇒ T) = T
tgt none = none
tgt any = any
tgt number = none
tgt boolean = none
tgt string = none
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
