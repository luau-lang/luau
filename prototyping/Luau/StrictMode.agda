{-# OPTIONS --rewriting #-}

module Luau.StrictMode where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Maybe using (just; nothing)
open import Luau.Syntax using (Expr; Stat; Block; yes; nil; addr; var; var_∈_; _⟨_⟩∈_; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name)
open import Luau.Type using (Type; strict; bot; top; nil; _⇒_; tgt)
open import Luau.Heap using (Heap; function_is_end) renaming (_[_] to _[_]ᴴ)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∈_; _⊢ᴱ_∈_; var; addr; app; block; return; local; function)
open import Properties.Equality using (_≢_)
open import Properties.TypeCheck(strict) using (typeOfᴴ; typeCheckᴮ)

src : Type → Type
src = Luau.Type.src strict

data Warningᴱ (H : Heap yes) {Γ} : ∀ {M T} → (Γ ⊢ᴱ M ∈ T) → Set
data Warningᴮ (H : Heap yes) {Γ} : ∀ {B T} → (Γ ⊢ᴮ B ∈ T) → Set

data Warningᴱ H {Γ} where

  BadlyTypedFunctionAddress : ∀ a f {x S T U B} →

    (H [ a ]ᴴ ≡ just (function f ⟨ var x ∈ T ⟩∈ U is B end)) →
    Warningᴮ H (typeCheckᴮ H (x ↦ T) B) →
    --------------------------------------------------------
    Warningᴱ H (addr a S)

  UnallocatedAddress : ∀ a {T} →

    (H [ a ]ᴴ ≡ nothing) →
    --------------------------------------------------------
    Warningᴱ H (addr a T)

  app₀ : ∀ {M N T U} {D₁ : Γ ⊢ᴱ M ∈ T} {D₂ : Γ ⊢ᴱ N ∈ U} →

    (src T ≢ U) →
    -----------------
    Warningᴱ H (app D₁ D₂)
  
  app₁ : ∀ {M N T U} {D₁ : Γ ⊢ᴱ M ∈ T} {D₂ : Γ ⊢ᴱ N ∈ U} →

    Warningᴱ H D₁ →
    -----------------
    Warningᴱ H (app D₁ D₂)

  app₂ : ∀ {M N T U} {D₁ : Γ ⊢ᴱ M ∈ T} {D₂ : Γ ⊢ᴱ N ∈ U} →

    Warningᴱ H D₂ →
    -----------------
    Warningᴱ H (app D₁ D₂)

  function₀ : ∀ f {x B T U V} {D : (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V} →

    (U ≢ V) →
    -------------------------
    Warningᴱ H (function f {U = U} D)

  function₁ : ∀ f {x B T U V} {D : (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V} →

    Warningᴮ H D →
    -------------------------
    Warningᴱ H (function f {U = U} D)

  block : ∀ b {B T} {D : Γ ⊢ᴮ B ∈ T} →

    Warningᴮ H D →
    -----------------
    Warningᴱ H (block b D)

data Warningᴮ H {Γ} where

  return : ∀ {M B T U} {D₁ : Γ ⊢ᴱ M ∈ T} {D₂ : Γ ⊢ᴮ B ∈ U} →

    Warningᴱ H D₁ →
    ------------------
    Warningᴮ H (return D₁ D₂)

  local₀ : ∀ {x M B T U V} {D₁ : Γ ⊢ᴱ M ∈ U} {D₂ : (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V} →

    (T ≢ U) →
    --------------------
    Warningᴮ H (local D₁ D₂)

  local₁ : ∀ {x M B T U V} {D₁ : Γ ⊢ᴱ M ∈ U} {D₂ : (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V} →

    Warningᴱ H D₁ →
    --------------------
    Warningᴮ H (local D₁ D₂)

  local₂ : ∀ {x M B T U V} {D₁ : Γ ⊢ᴱ M ∈ U} {D₂ : (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V} →

    Warningᴮ H D₂ →
    --------------------
    Warningᴮ H (local D₁ D₂)

  function₁ : ∀ f {x B C T U V W} {D₁ : (Γ ⊕ x ↦ T) ⊢ᴮ C ∈ V} {D₂ : (Γ ⊕ f ↦ (T ⇒ U)) ⊢ᴮ B ∈ W} →

    Warningᴮ H D₁ →
    --------------------
    Warningᴮ H (function f D₁ D₂)

  function₂ : ∀ f {x B C T U V W} {D₁ : (Γ ⊕ x ↦ T) ⊢ᴮ C ∈ V} {D₂ : (Γ ⊕ f ↦ (T ⇒ U)) ⊢ᴮ B ∈ W} →

    Warningᴮ H D₂ →
    --------------------
    Warningᴮ H (function f D₁ D₂)
