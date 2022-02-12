module Luau.VarCtxt where

open import Agda.Builtin.Equality using (_≡_)
open import Luau.Type using (Type; none; _∪_; _∩_)
open import Luau.Var using (Var)
open import FFI.Data.Aeson using (KeyMap; Key; empty; unionWith; singleton; insert; delete; lookup; fromString; lookup-insert)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Properties.Equality using (cong)

orNone : Maybe Type → Type
orNone nothing = none
orNone (just T) = T

VarCtxt : Set
VarCtxt = KeyMap Type

∅ : VarCtxt
∅ = empty

_⋒_ : VarCtxt → VarCtxt → VarCtxt
_⋒_ = unionWith _∩_

_⋓_ : VarCtxt → VarCtxt → VarCtxt
_⋓_ = unionWith _∪_

_[_] : VarCtxt → Var → Type
_[_] Γ x = orNone (lookup (fromString x) Γ)

_⊝_ : VarCtxt → Var → VarCtxt
Γ ⊝ x = delete (fromString x) Γ

_↦_ : Var → Type → VarCtxt
x ↦ T = singleton (fromString x) T

_⊕_↦_ : VarCtxt → Var → Type → VarCtxt
Γ ⊕ x ↦ T = insert (fromString x) T Γ

-- ⊕-[] : ∀ (Γ : VarCtxt) x T → (((Γ ⊕ x ↦ T) [ x ]) ≡ T)
⊕-[] = λ (Γ : VarCtxt) x T → cong orNone (lookup-insert (fromString x) T Γ)
