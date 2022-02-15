{-# OPTIONS --rewriting #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; HeapValue; function_is_end; defn; alloc; ok; next; lookup-next) renaming (_[_] to _[_]ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; bot; app₁; app₂; block; return; local₁)
open import Luau.Syntax using (Expr; yes; var_∈_; _⟨_⟩∈_; _$_; addr; nil; block_is_end; done; return; local_←_; _∙_; fun; arg)
open import Luau.Type using (Type; strict; nil; _⇒_; bot; tgt)
open import Luau.TypeCheck(strict) using (_▷_⊢ᴮ_∋_∈_⊣_; _▷_⊢ᴱ_∋_∈_⊣_; _▷_∈_; _▷_✓; nil; var; addr; app; function; block; done; return; local; nothing)
open import Luau.Value using (val; nil; addr)
open import Luau.AddrCtxt using (AddrCtxt)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_; ∅-[]) renaming (_[_] to _[_]ⱽ)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (cong)
open import Properties.TypeCheck(strict) using (typeOfᴱ; typeCheckᴱ)
open import Luau.OpSem using (_⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app; function; beta; return; block; done; local; subst)

{-# REWRITE ∅-[] #-}

heap-miss : ∀ {Σ HV T} → (Σ ▷ HV ∈ T) → (HV ≡ nothing) → (T ≡ bot)
heap-miss nothing refl = refl

data ProgressResultᴱ {Σ Γ S M T Δ} (H : Heap yes) (D : Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) : Set
data ProgressResultᴮ {Σ Γ S B T Δ} (H : Heap yes) (D : Σ ▷ Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ) : Set

data ProgressResultᴱ {Σ Γ S M T Δ} H D where

  value : ∀ V → (M ≡ val V) → ProgressResultᴱ H D
  warning : (Warningᴱ D) → ProgressResultᴱ H D
  step : ∀ {M′ H′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → ProgressResultᴱ H D

data ProgressResultᴮ {Σ Γ S B T Δ} H D where

  done : (B ≡ done) → ProgressResultᴮ H D
  return : ∀ V {C} → (B ≡ (return (val V) ∙ C)) → ProgressResultᴮ H D
  warning : (Warningᴮ D) → ProgressResultᴮ H D
  step : ∀ {B′ H′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → ProgressResultᴮ H D

progressᴱ : ∀ {Σ Γ S M T Δ} H → (Σ ▷ H ✓) → (D : Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) → (Γ ≡ ∅) → ProgressResultᴱ H D
progressᴮ : ∀ {Σ Γ S B T Δ} H → (Σ ▷ H ✓) → (D : Σ ▷ Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ) → (Γ ≡ ∅) → ProgressResultᴮ H D

progressᴱ H h nil _ = value nil refl
progressᴱ H h (var x p) refl = warning (bot p)
progressᴱ H h (addr a refl) _ = value (addr a) refl
progressᴱ H h (app D₁ D₂) p with progressᴱ H h D₁ p
progressᴱ H h (app nil D₂) p | value nil refl = warning (bot refl)
progressᴱ H h (app (var _ _) D₂) p | value nil ()
progressᴱ H h (app (app _ _) D₂) p | value nil ()
progressᴱ H h (app (function _) D₂) p | value nil ()
progressᴱ H h (app (block _ _) D₂) p | value nil ()
progressᴱ H h (app (addr _ refl) D₂) p | value (addr a) refl with remember(H [ a ]ᴴ)
progressᴱ H h (app (addr _ refl) D₂) p | value (addr a) refl | (nothing , r) = warning (bot (cong tgt (heap-miss (h a) r)))
progressᴱ H h (app (addr _ refl) D₂) p | value (addr a) refl | (just(function f ⟨ var x ∈ S ⟩∈ T is B end) , r) = step (beta r)
progressᴱ H h (app D₁ D₂) p | warning W = warning (app₁ W)
progressᴱ H h (app D₁ D₂) p | step S = step (app S)
progressᴱ H h (function D) _ with alloc H _
progressᴱ H h (function D) _ | ok a H′ r = step (function r)
progressᴱ H h (block b D) q with progressᴮ H h D q
progressᴱ H h (block b D) q | done refl = step done
progressᴱ H h (block b D) q | return V refl = step (return refl)
progressᴱ H h (block b D) q | warning W = warning (block b W)
progressᴱ H h (block b D) q | step S = step (block S)

progressᴮ H h done q = done refl
progressᴮ H h (return D₁ D₂) q with progressᴱ H h D₁ q
progressᴮ H h (return D₁ D₂) q | value V refl = return V refl
progressᴮ H h (return D₁ D₂) q | warning W = warning (return W)
progressᴮ H h (return D₁ D₂) q | step S = step (return S)
progressᴮ H h (local D₁ D₂) q with progressᴱ H h D₁ q
progressᴮ H h (local D₁ D₂) q | value V refl = step subst
progressᴮ H h (local D₁ D₂) q | warning W = warning (local₁ W)
progressᴮ H h (local D₁ D₂) q | step S = step (local S)
progressᴮ H h (function D₁ D₂) q with alloc H _
progressᴮ H h (function D₁ D₂) q | ok a H′ r = step (function r)
