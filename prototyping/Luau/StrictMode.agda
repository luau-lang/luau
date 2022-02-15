module Luau.StrictMode where

open import Agda.Builtin.Equality using (_≡_)
open import Luau.Syntax using (Expr; Stat; Block; yes; nil; addr; var; var_∈_; _⟨_⟩∈_; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name)
open import Luau.Type using (Type; strict; bot; top; nil; _⇒_; tgt)
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import Luau.TypeCheck(strict) using (_▷_⊢ᴮ_∋_∈_⊣_; _▷_⊢ᴱ_∋_∈_⊣_; var; addr; app; block; return; local)

src : Type → Type
src = Luau.Type.src strict

data Warningᴱ {Σ Γ S} : ∀ {M T Δ} → (Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) → Set
data Warningᴮ {Σ Γ S} : ∀ {B T Δ} → (Σ ▷ Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ) → Set

data Warningᴱ {Σ Γ S} where

  bot : ∀ {M T Δ} {D : Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ} →

    (T ≡ bot) →
    -----------
    Warningᴱ(D)

  app₁ : ∀ {M N T U Δ₁ Δ₂} {D₁ : Σ ▷ Γ ⊢ᴱ (U ⇒ S) ∋ M ∈ T ⊣ Δ₁} {D₂ : Σ ▷ Γ ⊢ᴱ (src T) ∋ N ∈ U ⊣ Δ₂} →

    Warningᴱ D₁ →
    -----------------
    Warningᴱ(app D₁ D₂)

  app₂ : ∀ {M N T U Δ₁ Δ₂} {D₁ : Σ ▷ Γ ⊢ᴱ (U ⇒ S) ∋ M ∈ T ⊣ Δ₁} {D₂ : Σ ▷ Γ ⊢ᴱ (src T) ∋ N ∈ U ⊣ Δ₂} →

    Warningᴱ D₂ →
    -----------------
    Warningᴱ(app D₁ D₂)

  block : ∀ b {B T Δ} {D : Σ ▷ Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ} →

    Warningᴮ D →
    -----------------
    Warningᴱ(block b D)

data Warningᴮ {Σ Γ S} where

  return : ∀ {M B T U Δ₁ Δ₂} {D₁ : Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ₁} {D₂ : Σ ▷ Γ ⊢ᴮ top ∋ B ∈ U ⊣ Δ₂} →

    Warningᴱ(D₁) →
    ------------------
    Warningᴮ(return D₁ D₂)

  local₁ : ∀ {x M B T U V Δ₁ Δ₂} {D₁ : Σ ▷ Γ ⊢ᴱ T ∋ M ∈ U ⊣ Δ₁} {D₂ : Σ ▷ (Γ ⊕ x ↦ T) ⊢ᴮ S ∋ B ∈ V ⊣ Δ₂} →

    Warningᴱ(D₁) →
    --------------------
    Warningᴮ(local D₁ D₂)
