module Properties.Step where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (just; nothing)
open import Luau.Heap using (Heap; lookup; alloc; ok; function_⟨_⟩_end)
open import Luau.Syntax using (Block; Expr; nil; var; addr; function⟨_⟩_end; block_is_end; _$_; local_←_; function_⟨_⟩_end; return; done; _∙_)
open import Luau.OpSem using (_⊢_⟶ᴱ_⊣_; _⊢_⟶ᴮ_⊣_; app ; beta; function; block; return; done; local; subst)
open import Luau.RuntimeError using (RuntimeErrorᴱ; RuntimeErrorᴮ; NilIsNotAFunction; UnboundVariable; SEGV; app; block; local; return)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Luau.Value using (nil; addr; val)
open import Properties.Remember using (remember; _,_)

data StepResultᴮ (H : Heap) (B : Block) : Set
data StepResultᴱ (H : Heap) (M : Expr) : Set

data StepResultᴮ H B where
  step : ∀ H′ B′ → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → StepResultᴮ H B
  return : ∀ V {B′} → (B ≡ (return (val V) ∙ B′)) → StepResultᴮ H B
  done : (B ≡ done) → StepResultᴮ H B
  error : (RuntimeErrorᴮ H B) → StepResultᴮ H B

data StepResultᴱ H M where
  step : ∀ H′ M′ → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → StepResultᴱ H M
  value : ∀ V → (M ≡ val V) → StepResultᴱ H M
  error : (RuntimeErrorᴱ H M) → StepResultᴱ H M

stepᴱ : ∀ H M → StepResultᴱ H M
stepᴮ : ∀ H B → StepResultᴮ H B

stepᴱ H nil = value nil refl
stepᴱ H (var x) = error (UnboundVariable x)
stepᴱ H (addr a) = value (addr a) refl
stepᴱ H (M $ N) with stepᴱ H M
stepᴱ H (M $ N) | step H′ M′ D = step H′ (M′ $ N) (app D)
stepᴱ H (nil $ N) | value nil refl = error NilIsNotAFunction
stepᴱ H (addr a $ N) | value (addr a) refl with remember (lookup H a)
stepᴱ H (addr a $ N) | value (addr a) refl | (nothing , p) = error (app (SEGV a p))
stepᴱ H (addr a $ N) | value (addr a) refl | (just(function f ⟨ x ⟩ B end) , p) = step H (block f is local x ← N ∙ B end) (beta p)
stepᴱ H (M $ N) | error E = error (app E)
stepᴱ H (function⟨ x ⟩ B end) with alloc H (function "anon" ⟨ x ⟩ B end)
stepᴱ H (function⟨ x ⟩ B end) | ok a H′ p = step H′ (addr a) (function p)
stepᴱ H (block b is B end) with stepᴮ H B
stepᴱ H (block b is B end) | step H′ B′ D = step H′ (block b is B′ end) (block D)
stepᴱ H (block b is (return _ ∙ B′) end) | return V refl = step H (val V) return
stepᴱ H (block b is done end) | done refl = step H nil done
stepᴱ H (block b is B end) | error E = error (block b E)

stepᴮ H (function f ⟨ x ⟩ C end ∙ B) with alloc H (function f ⟨ x ⟩ C end)
stepᴮ H (function f ⟨ x ⟩ C end ∙ B) | ok a H′ p = step H′ (B [ addr a / f ]ᴮ) (function p)
stepᴮ H (local x ← M ∙ B) with stepᴱ H M
stepᴮ H (local x ← M ∙ B) | step H′ M′ D = step H′ (local x ← M′ ∙ B) (local D)
stepᴮ H (local x ← _ ∙ B) | value V refl = step H (B [ V / x ]ᴮ) subst
stepᴮ H (local x ← M ∙ B) | error E = error (local x E)
stepᴮ H (return M ∙ B) with stepᴱ H M
stepᴮ H (return M ∙ B) | step H′ M′ D = step H′ (return M′ ∙ B) (return D)
stepᴮ H (return _ ∙ B) | value V refl = return V refl
stepᴮ H (return M ∙ B) | error E = error (return E)
stepᴮ H done = done refl
