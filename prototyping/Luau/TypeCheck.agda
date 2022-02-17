{-# OPTIONS --rewriting #-}

open import Luau.Type using (Mode)

module Luau.TypeCheck (m : Mode) where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Maybe using (Maybe; just)
open import Luau.Syntax using (Expr; Stat; Block; yes; nil; addr; var; var_∈_; _⟨_⟩∈_; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name)
open import Luau.Var using (Var)
open import Luau.Addr using (Addr)
open import Luau.Heap using (Heap; HeapValue; function_is_end) renaming (_[_] to _[_]ᴴ)
open import Luau.Value using (addr; val)
open import Luau.Type using (Type; Mode; nil; bot; top; _⇒_; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import FFI.Data.Vector using (Vector)
open import FFI.Data.Maybe using (Maybe; just; nothing)

src : Type → Type
src = Luau.Type.src m

data _⊢ᴮ_∈_ : VarCtxt → Block yes → Type → Set
data _⊢ᴱ_∈_ : VarCtxt → Expr yes → Type → Set

data _⊢ᴮ_∈_ where

  done : ∀ {Γ} →

    ---------------
    Γ ⊢ᴮ done ∈ nil

  return : ∀ {M B T U Γ} →

    Γ ⊢ᴱ M ∈ T →
    Γ ⊢ᴮ B ∈ U →
    ---------------------
    Γ ⊢ᴮ return M ∙ B ∈ T

  local : ∀ {x M B T U V Γ} →

    Γ ⊢ᴱ M ∈ U →
    (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V →
    --------------------------------
    Γ ⊢ᴮ local var x ∈ T ← M ∙ B ∈ V

  function : ∀ f {x B C T U V W Γ} →

    (Γ ⊕ x ↦ T) ⊢ᴮ C ∈ V →
    (Γ ⊕ f ↦ (T ⇒ U)) ⊢ᴮ B ∈ W →
    -------------------------------------------------
    Γ ⊢ᴮ function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B ∈ W

data _⊢ᴱ_∈_ where

  nil : ∀ {Γ} →

    --------------
    Γ ⊢ᴱ nil ∈ nil

  var : ∀ x {T Γ} →

    T ≡ Γ [ x ]ⱽ →
    --------------
    Γ ⊢ᴱ var x ∈ T

  addr : ∀ a T {Γ} →

    -----------------
    Γ ⊢ᴱ (addr a) ∈ T

  app : ∀ {M N T U Γ} →

    Γ ⊢ᴱ M ∈ T →
    Γ ⊢ᴱ N ∈ U →
    ----------------------
    Γ ⊢ᴱ (M $ N) ∈ (tgt T)

  function : ∀ f {x B T U V Γ} →

    (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V →
    -----------------------------------------------------
    Γ ⊢ᴱ (function f ⟨ var x ∈ T ⟩∈ U is B end) ∈ (T ⇒ U)

  block : ∀ b {B T Γ} →

    Γ ⊢ᴮ B ∈ T →
    ---------------------------
    Γ ⊢ᴱ (block b is B end) ∈ T

-- data _⊢ᴮ_∋_∈_⊣_ : VarCtxt → Type → Block yes → Type → VarCtxt → Set
-- data _⊢ᴱ_∋_∈_⊣_ : VarCtxt → Type → Expr yes → Type → VarCtxt → Set

-- data _⊢ᴮ_∋_∈_⊣_ where

--   done : ∀ {S Γ} →

--     ----------------------
--     Γ ⊢ᴮ S ∋ done ∈ nil ⊣ ∅

--   return : ∀ {M B S T U Γ Δ₁ Δ₂} →

--     Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ₁ →
--     Γ ⊢ᴮ nil ∋ B ∈ U ⊣ Δ₂ →
--     ---------------------------------
--     Γ ⊢ᴮ S ∋ return M ∙ B ∈ T ⊣ Δ₁

--   local : ∀ {x M B S T U V Γ Δ₁ Δ₂} →

--     Γ ⊢ᴱ T ∋ M ∈ U ⊣ Δ₁ →
--     (Γ ⊕ x ↦ T) ⊢ᴮ S ∋ B ∈ V ⊣ Δ₂ →
--     ----------------------------------------------------------
--     Γ ⊢ᴮ S ∋ local var x ∈ T ← M ∙ B ∈ V ⊣ (Δ₁ ⋒ (Δ₂ ⊝ x))

--   function : ∀ {f x B C S T U V W Γ Δ₁ Δ₂} →

--     (Γ ⊕ x ↦ T) ⊢ᴮ U ∋ C ∈ V ⊣ Δ₁ →
--     (Γ ⊕ f ↦ (T ⇒ U)) ⊢ᴮ S ∋ B ∈ W ⊣ Δ₂ →
--     ---------------------------------------------------------------------------------
--     Γ ⊢ᴮ S ∋ function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B ∈ W ⊣ ((Δ₁ ⊝ x) ⋒ (Δ₂ ⊝ f))

-- data _⊢ᴱ_∋_∈_⊣_  where

--   nil : ∀ {S Γ} →

--     ----------------------
--     Γ ⊢ᴱ S ∋ nil ∈ nil ⊣ ∅

--   var : ∀ x {S T Γ} →

--     T ≡ Γ [ x ]ⱽ →
--     ----------------------------
--     Γ ⊢ᴱ S ∋ var x ∈ T ⊣ (x ↦ S)

--   addr : ∀ a T {S Γ} →

--     -------------------------
--     Γ ⊢ᴱ S ∋ (addr a) ∈ T ⊣ ∅

--   app : ∀ {M N S T U Γ Δ₁ Δ₂} →

--     Γ ⊢ᴱ (U ⇒ S) ∋ M ∈ T ⊣ Δ₁ →
--     Γ ⊢ᴱ (src T) ∋ N ∈ U ⊣ Δ₂ →
--     --------------------------------------
--     Γ ⊢ᴱ S ∋ (M $ N) ∈ (tgt T) ⊣ (Δ₁ ⋒ Δ₂)

--   function : ∀ {f x B S T U V Γ Δ} →

--     (Γ ⊕ x ↦ T) ⊢ᴮ U ∋ B ∈ V ⊣ Δ →
--     -----------------------------------------------------------------------
--     Γ ⊢ᴱ S ∋ (function f ⟨ var x ∈ T ⟩∈ U is B end) ∈ (T ⇒ U) ⊣ (Δ ⊝ x)

--   block : ∀ b {B S T Γ Δ} →

--     Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ →
--     ----------------------------------------------------
--     Γ ⊢ᴱ S ∋ (block b is B end) ∈ T ⊣ Δ
