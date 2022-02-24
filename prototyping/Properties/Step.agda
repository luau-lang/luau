{-# OPTIONS --rewriting #-}

module Properties.Step where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Float using (primFloatPlus; primFloatMinus; primFloatTimes; primFloatDiv)
open import FFI.Data.Maybe using (just; nothing)
open import Luau.Heap using (Heap; _[_]; alloc; ok; function_is_end)
open import Luau.Syntax using (Block; Expr; nil; var; addr; function_is_end; block_is_end; _$_; local_←_; return; done; _∙_; name; fun; arg; number; binexp; +)
open import Luau.OpSem using (_⊢_⟶ᴱ_⊣_; _⊢_⟶ᴮ_⊣_; app₁ ; app₂ ; beta; function; block; return; done; local; subst; binOpEval; evalBinOp; binOp₁; binOp₂)
open import Luau.RuntimeError using (RuntimeErrorᴱ; RuntimeErrorᴮ; FunctionMismatch; BinopMismatch₁; BinopMismatch₂; UnboundVariable; SEGV; app₁; app₂; block; local; return; bin₁; bin₂)
open import Luau.RuntimeType using (function; number)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Luau.Value using (nil; addr; val; number)
open import Properties.Remember using (remember; _,_)

data StepResultᴮ {a} (H : Heap a) (B : Block a) : Set
data StepResultᴱ {a} (H : Heap a) (M : Expr a) : Set

data StepResultᴮ H B where
  step : ∀ H′ B′ → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → StepResultᴮ H B
  return : ∀ V {B′} → (B ≡ (return (val V) ∙ B′)) → StepResultᴮ H B
  done : (B ≡ done) → StepResultᴮ H B
  error : (RuntimeErrorᴮ H B) → StepResultᴮ H B

data StepResultᴱ H M where
  step : ∀ H′ M′ → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → StepResultᴱ H M
  value : ∀ V → (M ≡ val V) → StepResultᴱ H M
  error : (RuntimeErrorᴱ H M) → StepResultᴱ H M

stepᴱ : ∀ {a} H M → StepResultᴱ {a} H M
stepᴮ : ∀ {a} H B → StepResultᴮ {a} H B

stepᴱ H nil = value nil refl
stepᴱ H (var x) = error UnboundVariable
stepᴱ H (addr a) = value (addr a) refl
stepᴱ H (number x) = value (number x) refl
stepᴱ H (M $ N) with stepᴱ H M
stepᴱ H (M $ N) | step H′ M′ D = step H′ (M′ $ N) (app₁ D)
stepᴱ H (_ $ N) | value v refl with stepᴱ H N
stepᴱ H (_ $ N) | value v refl | step H′ N′ s = step H′ (val v $ N′) (app₂ v s)
stepᴱ H (_ $ _) | value (addr a) refl | value w refl with remember (H [ a ])
stepᴱ H (_ $ _) | value (addr a) refl | value w refl  | (nothing , p) = error (app₁ (SEGV p))
stepᴱ H (_ $ _) | value (addr a) refl | value w refl  | (just(function F is B end) , p) = step H (block (fun F) is B [ w / name (arg F) ]ᴮ end) (beta function F is B end w refl p)
stepᴱ H (_ $ _) | value nil refl | value w refl = error (FunctionMismatch nil w (λ ()))
stepᴱ H (_ $ _) | value (number x) refl | value w refl = error (FunctionMismatch (number x) w (λ ()))
stepᴱ H (M $ N) | value V p | error E = error (app₂ E)
stepᴱ H (M $ N) | error E = error (app₁ E)
stepᴱ H (block b is B end) with stepᴮ H B
stepᴱ H (block b is B end) | step H′ B′ D = step H′ (block b is B′ end) (block D)
stepᴱ H (block b is (return _ ∙ B′) end) | return v refl = step H (val v) (return v)
stepᴱ H (block b is done end) | done refl = step H nil done
stepᴱ H (block b is B end) | error E = error (block E)
stepᴱ H (function F is C end) with alloc H (function F is C end)
stepᴱ H function F is C end | ok a H′ p = step H′ (addr a) (function a p)
stepᴱ H (binexp M op N) with stepᴱ H M
stepᴱ H (binexp M op N) | value v refl with stepᴱ H N
stepᴱ H (binexp M op N) | value v refl | step H′ N′ s = step H′ (binexp (val v) op N′) (binOp₂ s)
stepᴱ H (binexp M op N) | value v refl | error E = error (bin₂ E)
stepᴱ H (binexp M op N) | value (number m) refl | value (number n) refl = step H (number (evalBinOp m op n)) binOpEval
stepᴱ H (binexp M op N) | value nil refl | value w refl = error (BinopMismatch₁ nil w λ ())
stepᴱ H (binexp M op N) | value (addr a) refl | value w refl = error (BinopMismatch₁ (addr a) w λ ())
stepᴱ H (binexp M op N) | value v refl | value nil refl = error (BinopMismatch₂ v nil (λ ()))
stepᴱ H (binexp M op N) | value v refl | value (addr a) refl = error (BinopMismatch₂ v (addr a) (λ ()))
stepᴱ H (binexp M op N) | step H′ M′ s = step H′ (binexp M′ op N) (binOp₁ s)
stepᴱ H (binexp M op N) | error E = error (bin₁ E)

stepᴮ H (function F is C end ∙ B) with alloc H (function F is C end)
stepᴮ H (function F is C end ∙ B) | ok a H′ p = step H′ (B [ addr a / name (fun F) ]ᴮ) (function a p)
stepᴮ H (local x ← M ∙ B) with stepᴱ H M
stepᴮ H (local x ← M ∙ B) | step H′ M′ D = step H′ (local x ← M′ ∙ B) (local D)
stepᴮ H (local x ← _ ∙ B) | value v refl = step H (B [ v / name x ]ᴮ) (subst v)
stepᴮ H (local x ← M ∙ B) | error E = error (local E)
stepᴮ H (return M ∙ B) with stepᴱ H M
stepᴮ H (return M ∙ B) | step H′ M′ D = step H′ (return M′ ∙ B) (return D)
stepᴮ H (return _ ∙ B) | value V refl = return V refl
stepᴮ H (return M ∙ B) | error E = error (return E)
stepᴮ H done = done refl
