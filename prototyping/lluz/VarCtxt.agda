{-# OPTIONS --rewriting #-}

module Luau.VarCtxt where

open import Agda.Builtin.Equality using (_≡_)
open import Luau.Type using (Type; _∪_; _∩_)
open import Luau.Var using (Var)
open import FFI.Data.Aeson using (KeyMap; Key; empty; unionWith; singleton; insert; delete; lookup; toString; fromString; lookup-insert; lookup-insert-not; lookup-empty; to-from; insert-swap; insert-over)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Properties.Equality using (_≢_; cong; sym; trans)

VarCtxt : Set
VarCtxt = KeyMap Type

∅ : VarCtxt
∅ = empty

_⋒_ : VarCtxt → VarCtxt → VarCtxt
_⋒_ = unionWith _∩_

_⋓_ : VarCtxt → VarCtxt → VarCtxt
_⋓_ = unionWith _∪_

_[_] : VarCtxt → Var → Maybe Type
Γ [ x ] = lookup (fromString x) Γ

_⊝_ : VarCtxt → Var → VarCtxt
Γ ⊝ x = delete (fromString x) Γ

_↦_ : Var → Type → VarCtxt
x ↦ T = singleton (fromString x) T

_⊕_↦_ : VarCtxt → Var → Type → VarCtxt
Γ ⊕ x ↦ T = insert (fromString x) T Γ

⊕-over : ∀ {Γ x y T U} → (x ≡ y) → ((Γ ⊕ x ↦ T) ⊕ y ↦ U) ≡ (Γ ⊕ y ↦ U)
⊕-over p = insert-over _ _ _ _ _ (cong fromString (sym p))

⊕-swap : ∀ {Γ x y T U} → (x ≢ y) → ((Γ ⊕ x ↦ T) ⊕ y ↦ U) ≡ ((Γ ⊕ y ↦ U) ⊕ x ↦ T)
⊕-swap p = insert-swap _ _ _ _ _ (λ q → p (trans (sym (to-from _)) (trans (cong toString (sym q) ) (to-from _))) )

⊕-lookup-miss : ∀ x y T Γ → (x ≢ y) → (Γ [ y ] ≡ (Γ ⊕ x ↦ T) [ y ])
⊕-lookup-miss x y T Γ p = lookup-insert-not (fromString x) (fromString y) T Γ λ q → p (trans (sym (to-from x)) (trans (cong toString q) (to-from y)))
