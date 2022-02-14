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
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import FFI.Data.Vector using (Vector)
open import FFI.Data.Maybe using (Maybe; just; nothing)

src : Type → Type
src = Luau.Type.src m

data _▷_⊢ᴮ_∋_∈_⊣_ : AddrCtxt → VarCtxt → Type → Block yes → Type → VarCtxt → Set
data _▷_⊢ᴱ_∋_∈_⊣_ : AddrCtxt → VarCtxt → Type → Expr yes → Type → VarCtxt → Set

data _▷_⊢ᴮ_∋_∈_⊣_ where

  done : ∀ {Σ S Γ} →

    ----------------------
    Σ ▷ Γ ⊢ᴮ S ∋ done ∈ nil ⊣ ∅

  return : ∀ {Σ M B S T U Γ Δ₁ Δ₂} →

    Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ₁ →
    Σ ▷ Γ ⊢ᴮ top ∋ B ∈ U ⊣ Δ₂ →
    ---------------------------------
    Σ ▷ Γ ⊢ᴮ S ∋ return M ∙ B ∈ T ⊣ Δ₁

  local : ∀ {Σ x M B S T U V Γ Δ₁ Δ₂} →

    Σ ▷ Γ ⊢ᴱ T ∋ M ∈ U ⊣ Δ₁ →
    Σ ▷ (Γ ⊕ x ↦ T) ⊢ᴮ S ∋ B ∈ V ⊣ Δ₂ →
    ----------------------------------------------------------
    Σ ▷ Γ ⊢ᴮ S ∋ local var x ∈ T ← M ∙ B ∈ V ⊣ (Δ₁ ⋒ (Δ₂ ⊝ x))

  function : ∀ {Σ f x B C S T U V W Γ Δ₁ Δ₂} →

    Σ ▷ (Γ ⊕ x ↦ T) ⊢ᴮ U ∋ C ∈ V ⊣ Δ₁ →
    Σ ▷ (Γ ⊕ f ↦ (T ⇒ U)) ⊢ᴮ S ∋ B ∈ W ⊣ Δ₂ →
    ---------------------------------------------------------------------------------
    Σ ▷ Γ ⊢ᴮ S ∋ function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B ∈ W ⊣ ((Δ₁ ⊝ x) ⋒ (Δ₂ ⊝ f))

data _▷_⊢ᴱ_∋_∈_⊣_  where

  nil : ∀ {Σ S Γ} →

    ----------------------
    Σ ▷ Γ ⊢ᴱ S ∋ nil ∈ nil ⊣ ∅

  var : ∀ x {Σ S T Γ} →

    T ≡ Γ [ x ]ⱽ →
    ----------------------------
    Σ ▷ Γ ⊢ᴱ S ∋ var x ∈ T ⊣ (x ↦ S)

  addr : ∀ a {Σ S T Γ} →

    T ≡ Σ [ a ]ᴬ →
    ----------------------------
    Σ ▷ Γ ⊢ᴱ S ∋ addr a ∈ T ⊣ ∅

  app : ∀ {Σ M N S T U Γ Δ₁ Δ₂} →

    Σ ▷ Γ ⊢ᴱ (U ⇒ S) ∋ M ∈ T ⊣ Δ₁ →
    Σ ▷ Γ ⊢ᴱ (src T) ∋ N ∈ U ⊣ Δ₂ →
    --------------------------------------
    Σ ▷ Γ ⊢ᴱ S ∋ (M $ N) ∈ (tgt T) ⊣ (Δ₁ ⋒ Δ₂)

  function : ∀ {Σ f x B S T U V Γ Δ} →

    Σ ▷ (Γ ⊕ x ↦ T) ⊢ᴮ U ∋ B ∈ V ⊣ Δ →
    -----------------------------------------------------------------------
    Σ ▷ Γ ⊢ᴱ S ∋ (function f ⟨ var x ∈ T ⟩∈ U is B end) ∈ (T ⇒ U) ⊣ (Δ ⊝ x)

  block : ∀ b {Σ B S T Γ Δ} →

    Σ ▷ Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ →
    ----------------------------------------------------
    Σ ▷ Γ ⊢ᴱ S ∋ (block b is B end) ∈ T ⊣ Δ

data _▷_∈_ (Σ : AddrCtxt) : Maybe (HeapValue yes) → Type → Set where

  nothing :

    -----------------
    Σ ▷ nothing ∈ bot

  function : ∀ {f x B T U V W} →

    Σ ▷ (x ↦ T) ⊢ᴮ U ∋ B ∈ V ⊣ (x ↦ W) →
    ---------------------------------------------------------
    Σ ▷ just (function f ⟨ var x ∈ T ⟩∈ U is B end) ∈ (T ⇒ U)

_▷_✓ : AddrCtxt → Heap yes → Set
(Σ ▷ H ✓) = (∀ a → Σ ▷ (H [ a ]ᴴ) ∈ (Σ [ a ]ᴬ)) 
