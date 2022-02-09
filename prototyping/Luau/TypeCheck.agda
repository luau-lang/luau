module Luau.TypeCheck where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Maybe using (Maybe; just)
open import Luau.Syntax using (Expr; Stat; Block; nil; addr; var; function⟨_⟩_end; _$_; block_is_end; local_←_; _∙_; done; function_⟨_⟩_end; return; name)
open import Luau.Var using (Var)
open import Luau.Value using (addr; val)
open import Luau.Type using (Type; nil; _⇒_; src; tgt)
open import FFI.Data.Aeson using (KeyMap; Key)

Context : Set
Context = KeyMap Type

∅ : Context
∅ = {!!}

_⋒_ : Context → Context → Context
_⋒_ = {!!}

lookup : Context → Var → Maybe Type
lookup = {!!}

_↦_ : Var → Type → Context
_↦_ = {!!}

data _⊢ᴮ_∋_∈_⊣_ : Context → Type → Block → Type → Context → Set
data _⊢ᴱ_∋_∈_⊣_ : Context → Type → Expr → Type → Context → Set

data _⊢ᴮ_∋_∈_⊣_ where

data _⊢ᴱ_∋_∈_⊣_  where

  nil : ∀ {S Γ} →

    ----------------------
    Γ ⊢ᴱ S ∋ nil ∈ nil ⊣ ∅

  var : ∀ x {S T Γ} →

    just T ≡ lookup Γ x →
    ----------------------------
    Γ ⊢ᴱ S ∋ var x ∈ T ⊣ (x ↦ T)

  app : ∀ {M N S T U Γ Δ₁ Δ₂} →

    Γ ⊢ᴱ (U ⇒ S) ∋ M ∈ U ⊣ Δ₂ →
    Γ ⊢ᴱ (src S) ∋ N ∈ U ⊣ Δ₂ →
    --------------------------------------
    Γ ⊢ᴱ S ∋ (M $ N) ∈ (tgt T) ⊣ (Δ₁ ⋒ Δ₂)

