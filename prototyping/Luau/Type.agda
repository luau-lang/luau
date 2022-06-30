module Luau.Type where

open import FFI.Data.Maybe using (Maybe; just; nothing; just-inv)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Equality using (cong)
open import FFI.Data.Maybe using (Maybe; just; nothing)

data Type : Set where
  nil : Type
  _⇒_ : Type → Type → Type
  never : Type
  unknown : Type
  boolean : Type
  number : Type
  string : Type
  _∪_ : Type → Type → Type
  _∩_ : Type → Type → Type

data Scalar : Type → Set where

  number : Scalar number
  boolean : Scalar boolean
  string : Scalar string
  nil : Scalar nil

skalar = number ∪ (string ∪ (nil ∪ boolean))

lhs : Type → Type
lhs (T ⇒ _) = T
lhs (T ∪ _) = T
lhs (T ∩ _) = T
lhs nil = nil
lhs never = never
lhs unknown = unknown
lhs number = number
lhs boolean = boolean
lhs string = string

rhs : Type → Type
rhs (_ ⇒ T) = T
rhs (_ ∪ T) = T
rhs (_ ∩ T) = T
rhs nil = nil
rhs never = never
rhs unknown = unknown
rhs number = number
rhs boolean = boolean
rhs string = string

_≡ᵀ_ : ∀ (T U : Type) → Dec(T ≡ U)
nil ≡ᵀ nil = yes refl
nil ≡ᵀ (S ⇒ T) = no (λ ())
nil ≡ᵀ never = no (λ ())
nil ≡ᵀ unknown = no (λ ())
nil ≡ᵀ number = no (λ ())
nil ≡ᵀ boolean = no (λ ())
nil ≡ᵀ (S ∪ T) = no (λ ())
nil ≡ᵀ (S ∩ T) = no (λ ())
nil ≡ᵀ string = no (λ ())
(S ⇒ T) ≡ᵀ string = no (λ ())
never ≡ᵀ string = no (λ ())
unknown ≡ᵀ string = no (λ ())
boolean ≡ᵀ string = no (λ ())
number ≡ᵀ string = no (λ ())
(S ∪ T) ≡ᵀ string = no (λ ())
(S ∩ T) ≡ᵀ string = no (λ ())
(S ⇒ T) ≡ᵀ nil = no (λ ())
(S ⇒ T) ≡ᵀ (U ⇒ V) with (S ≡ᵀ U) | (T ≡ᵀ V) 
(S ⇒ T) ≡ᵀ (S ⇒ T) | yes refl | yes refl = yes refl
(S ⇒ T) ≡ᵀ (U ⇒ V) | _ | no p = no (λ q → p (cong rhs q))
(S ⇒ T) ≡ᵀ (U ⇒ V) | no p | _ = no (λ q → p (cong lhs q))
(S ⇒ T) ≡ᵀ never = no (λ ())
(S ⇒ T) ≡ᵀ unknown = no (λ ())
(S ⇒ T) ≡ᵀ number = no (λ ())
(S ⇒ T) ≡ᵀ boolean = no (λ ())
(S ⇒ T) ≡ᵀ (U ∪ V) = no (λ ())
(S ⇒ T) ≡ᵀ (U ∩ V) = no (λ ())
never ≡ᵀ nil = no (λ ())
never ≡ᵀ (U ⇒ V) = no (λ ())
never ≡ᵀ never = yes refl
never ≡ᵀ unknown = no (λ ())
never ≡ᵀ number = no (λ ())
never ≡ᵀ boolean = no (λ ())
never ≡ᵀ (U ∪ V) = no (λ ())
never ≡ᵀ (U ∩ V) = no (λ ())
unknown ≡ᵀ nil = no (λ ())
unknown ≡ᵀ (U ⇒ V) = no (λ ())
unknown ≡ᵀ never = no (λ ())
unknown ≡ᵀ unknown = yes refl
unknown ≡ᵀ number = no (λ ())
unknown ≡ᵀ boolean = no (λ ())
unknown ≡ᵀ (U ∪ V) = no (λ ())
unknown ≡ᵀ (U ∩ V) = no (λ ())
number ≡ᵀ nil = no (λ ())
number ≡ᵀ (T ⇒ U) = no (λ ())
number ≡ᵀ never = no (λ ())
number ≡ᵀ unknown = no (λ ())
number ≡ᵀ number = yes refl
number ≡ᵀ boolean = no (λ ())
number ≡ᵀ (T ∪ U) = no (λ ())
number ≡ᵀ (T ∩ U) = no (λ ())
boolean ≡ᵀ nil = no (λ ())
boolean ≡ᵀ (T ⇒ U) = no (λ ())
boolean ≡ᵀ never = no (λ ())
boolean ≡ᵀ unknown = no (λ ())
boolean ≡ᵀ boolean = yes refl
boolean ≡ᵀ number = no (λ ())
boolean ≡ᵀ (T ∪ U) = no (λ ())
boolean ≡ᵀ (T ∩ U) = no (λ ())
string ≡ᵀ nil = no (λ ())
string ≡ᵀ (x ⇒ x₁) = no (λ ())
string ≡ᵀ never = no (λ ())
string ≡ᵀ unknown = no (λ ())
string ≡ᵀ boolean = no (λ ())
string ≡ᵀ number = no (λ ())
string ≡ᵀ string = yes refl
string ≡ᵀ (U ∪ V) = no (λ ())
string ≡ᵀ (U ∩ V) = no (λ ())
(S ∪ T) ≡ᵀ nil = no (λ ())
(S ∪ T) ≡ᵀ (U ⇒ V) = no (λ ())
(S ∪ T) ≡ᵀ never = no (λ ())
(S ∪ T) ≡ᵀ unknown = no (λ ())
(S ∪ T) ≡ᵀ number = no (λ ())
(S ∪ T) ≡ᵀ boolean = no (λ ())
(S ∪ T) ≡ᵀ (U ∪ V) with (S ≡ᵀ U) | (T ≡ᵀ V) 
(S ∪ T) ≡ᵀ (S ∪ T) | yes refl | yes refl = yes refl
(S ∪ T) ≡ᵀ (U ∪ V) | _ | no p = no (λ q → p (cong rhs q))
(S ∪ T) ≡ᵀ (U ∪ V) | no p | _ = no (λ q → p (cong lhs q))
(S ∪ T) ≡ᵀ (U ∩ V) = no (λ ())
(S ∩ T) ≡ᵀ nil = no (λ ())
(S ∩ T) ≡ᵀ (U ⇒ V) = no (λ ())
(S ∩ T) ≡ᵀ never = no (λ ())
(S ∩ T) ≡ᵀ unknown = no (λ ())
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
