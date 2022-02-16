module Luau.StrictMode where

open import Agda.Builtin.Equality using (_≡_)
open import Luau.Syntax using (Expr; Stat; Block; yes; nil; addr; var; var_∈_; _⟨_⟩∈_; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name)
open import Luau.Type using (Type; strict; bot; top; nil; _⇒_; tgt)
open import Luau.Heap using (Heap) renaming (_[_] to _[_]ᴴ)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∋_∈_⊣_; _⊢ᴱ_∋_∈_⊣_; var; addr; app; block; return; local; function)
open import Properties.Equality using (_≢_)
open import Properties.TypeCheck(strict) using (typeOfᴴ)

src : Type → Type
src = Luau.Type.src strict

data Warningᴱ (H : Heap yes) {Γ S} : ∀ {M T Δ} → (Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) → Set
data Warningᴮ (H : Heap yes) {Γ S} : ∀ {B T Δ} → (Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ) → Set

data Warningᴱ H {Γ S} where

  bot : ∀ {M T Δ} {D : Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ} →

    (T ≡ bot) →
    ------------
    Warningᴱ H D

  addr : ∀ a T →

    (T ≢ typeOfᴴ(H [ a ]ᴴ)) →
    -------------------------
    Warningᴱ H (addr a T)

  app₁ : ∀ {M N T U Δ₁ Δ₂} {D₁ : Γ ⊢ᴱ (U ⇒ S) ∋ M ∈ T ⊣ Δ₁} {D₂ : Γ ⊢ᴱ (src T) ∋ N ∈ U ⊣ Δ₂} →

    Warningᴱ H D₁ →
    -----------------
    Warningᴱ H (app D₁ D₂)

  app₂ : ∀ {M N T U Δ₁ Δ₂} {D₁ : Γ ⊢ᴱ (U ⇒ S) ∋ M ∈ T ⊣ Δ₁} {D₂ : Γ ⊢ᴱ (src T) ∋ N ∈ U ⊣ Δ₂} →

    Warningᴱ H D₂ →
    -----------------
    Warningᴱ H(app D₁ D₂)

  block : ∀ b {B T Δ} {D : Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ} →

    Warningᴮ H D →
    -----------------
    Warningᴱ H(block b D)

data Warningᴮ H {Γ S} where

  disagree : ∀ {B T Δ} {D : Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ} →

    (S ≢ T) →
    -----------
    Warningᴮ H D

  return : ∀ {M B T U Δ₁ Δ₂} {D₁ : Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ₁} {D₂ : Γ ⊢ᴮ nil ∋ B ∈ U ⊣ Δ₂} →

    Warningᴱ H D₁ →
    ------------------
    Warningᴮ H (return D₁ D₂)

  local₁ : ∀ {x M B T U V Δ₁ Δ₂} {D₁ : Γ ⊢ᴱ T ∋ M ∈ U ⊣ Δ₁} {D₂ : (Γ ⊕ x ↦ T) ⊢ᴮ S ∋ B ∈ V ⊣ Δ₂} →

    Warningᴱ H D₁ →
    --------------------
    Warningᴮ H (local D₁ D₂)

-- data Warningᴴ {H} : ∀ {V T} → (H ▷ V ∈ T) → Set where

--   nothing :

--     -----------------
--     Warningᴴ(nothing)

--   function : ∀ f {x B T U V W} {D : (x ↦ T) ⊢ᴮ U ∋ B ∈ V ⊣ (x ↦ W)} →

--     Warningᴮ(D) →
--     --------------------
--     Warningᴴ(function f D)
