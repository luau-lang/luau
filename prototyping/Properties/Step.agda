{-# OPTIONS --rewriting #-}

module Properties.Step where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Float using (primFloatPlus; primFloatMinus; primFloatTimes; primFloatDiv; primFloatEquality; primFloatLess)
open import Agda.Builtin.Bool using (true; false)
open import Agda.Builtin.String using (primStringAppend)
open import FFI.Data.Maybe using (just; nothing)
open import Luau.Heap using (Heap; _[_]; alloc; ok; function_is_end)
open import Luau.Syntax using (Block; Expr; nil; var; val; addr; bool; function_is_end; block_is_end; _$_; local_←_; return; done; _∙_; name; fun; arg; number; binexp; +; -; *; /; <; >; <=; >=; ==; ~=; ··; string)
open import Luau.OpSem using (_⟦_⟧_⟶_; _⊢_⟶ᴱ_⊣_; _⊢_⟶ᴮ_⊣_; app₁ ; app₂ ; beta; function; block; return; done; local; subst; binOp₀; binOp₁; binOp₂; +; -; *; /; <; >; <=; >=; ==; ~=; ··; evalEqOp; evalNeqOp)
open import Luau.RuntimeError using (BinOpError; RuntimeErrorᴱ; RuntimeErrorᴮ; FunctionMismatch; BinOpMismatch₁; BinOpMismatch₂; UnboundVariable; SEGV; app₁; app₂; block; local; return; bin₁; bin₂; +; -; *; /; <; >; <=; >=; ··)
open import Luau.RuntimeType using (valueType; function; number)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Properties.Remember using (remember; _,_)
open import Utility.Bool using (not; _or_)

data BinOpStepResult v op w : Set where
  step : ∀ x → (v ⟦ op ⟧ w ⟶ x) → BinOpStepResult v op w
  error₁ : BinOpError op (valueType(v)) → BinOpStepResult v op w
  error₂ : BinOpError op (valueType(w)) → BinOpStepResult v op w

binOpStep : ∀ v op w → BinOpStepResult v op w
binOpStep nil + w = error₁ (+ (λ ()))
binOpStep (addr a) + w = error₁ (+ (λ ()))
binOpStep (number m) + nil = error₂ (+ (λ ()))
binOpStep (number m) + (addr a) = error₂ (+ (λ ()))
binOpStep (number m) + (number n) = step (number (primFloatPlus m n)) (+ m n)
binOpStep (number m) + (bool b) = error₂ (+ (λ ()))
binOpStep (number m) + (string x) = error₂ (+ (λ ()))
binOpStep (number m) - (string x) = error₂ (- (λ ()))
binOpStep (number m) * (string x) = error₂ (* (λ ()))
binOpStep (number m) / (string x) = error₂ (/ (λ ()))
binOpStep (number m) < (string x) = error₂ (< (λ ()))
binOpStep (number m) > (string x) = error₂ (> (λ ()))
binOpStep (number m) == (string x) = step (bool false) (== (number m) (string x))
binOpStep (number m) ~= (string x) = step (bool true) (~= (number m) (string x))
binOpStep (number m) <= (string x) = error₂ (<= (λ ()))
binOpStep (number m) >= (string x) = error₂ (>= (λ ()))
binOpStep (bool b) + w = error₁ (+ (λ ()))
binOpStep nil - w = error₁ (- (λ ()))
binOpStep (addr a) - w = error₁ (- (λ ()))
binOpStep (number x) - nil = error₂ (- (λ ()))
binOpStep (number x) - (addr a) = error₂ (- (λ ()))
binOpStep (number x) - (number n) = step (number (primFloatMinus x n)) (- x n)
binOpStep (number x) - (bool b) = error₂ (- (λ ()))
binOpStep (bool b) - w = error₁ (- (λ ()))
binOpStep nil * w = error₁ (* (λ ()))
binOpStep (addr a) * w = error₁ (* (λ ()))
binOpStep (number m) * nil = error₂ (* (λ ()))
binOpStep (number m) * (addr a) = error₂ (* (λ ()))
binOpStep (number m) * (number n) = step (number (primFloatDiv m n)) (* m n)
binOpStep (number m) * (bool b) = error₂ (* (λ ()))
binOpStep (bool b) * w = error₁ (* (λ ()))
binOpStep nil / w = error₁ (/ (λ ()))
binOpStep (addr a) / w = error₁ (/ (λ ()))
binOpStep (number m) / nil = error₂ (/ (λ ()))
binOpStep (number m) / (addr a) = error₂ (/ (λ ()))
binOpStep (number m) / (number n) = step (number (primFloatTimes m n)) (/ m n)
binOpStep (number m) / (bool b) = error₂ (/ (λ ()))
binOpStep (bool b) / w = error₁ (/ (λ ()))
binOpStep nil < w = error₁ (< (λ ()))
binOpStep (addr a) < w = error₁ (< (λ ()))
binOpStep (number m) < nil = error₂ (< (λ ()))
binOpStep (number m) < (addr a) = error₂ (< (λ ()))
binOpStep (number m) < (number n) = step (bool (primFloatLess m n)) (< m n)
binOpStep (number m) < (bool b) = error₂ (< (λ ()))
binOpStep (bool b) < w = error₁ (< (λ ()))
binOpStep nil > w = error₁ (> (λ ()))
binOpStep (addr a) > w = error₁ (> (λ ()))
binOpStep (number m) > nil = error₂ (> (λ ()))
binOpStep (number m) > (addr a) = error₂ (> (λ ()))
binOpStep (number m) > (number n) = step (bool (primFloatLess n m)) (> m n)
binOpStep (number m) > (bool b) = error₂ (> (λ ()))
binOpStep (bool b) > w = error₁ (> (λ ()))
binOpStep v == w = step (bool (evalEqOp v w)) (== v w)
binOpStep v ~= w = step (bool (evalNeqOp v w)) (~= v w)
binOpStep nil <= w = error₁ (<= (λ ()))
binOpStep (addr a) <= w = error₁ (<= (λ ()))
binOpStep (number m) <= nil = error₂ (<= (λ ()))
binOpStep (number m) <= (addr a) = error₂ (<= (λ ()))
binOpStep (number m) <= (number n) = step (bool (primFloatLess m n or primFloatEquality m n)) (<= m n)
binOpStep (number m) <= (bool b) = error₂ (<= (λ ()))
binOpStep (bool b) <= w = error₁ (<= (λ ()))
binOpStep nil >= w = error₁ (>= (λ ()))
binOpStep (addr a) >= w = error₁ (>= (λ ()))
binOpStep (number m) >= nil = error₂ (>= (λ ()))
binOpStep (number m) >= (addr a) = error₂ (>= (λ ()))
binOpStep (number m) >= (number n) = step (bool (primFloatLess n m or primFloatEquality m n)) (>= m n)
binOpStep (number m) >= (bool b) = error₂ (>= (λ ()))
binOpStep (bool b) >= w = error₁ (>= (λ ()))
binOpStep (string x) + w = error₁ (+ (λ ()))
binOpStep (string x) - w = error₁ (- (λ ()))
binOpStep (string x) * w = error₁ (* (λ ()))
binOpStep (string x) / w = error₁ (/ (λ ()))
binOpStep (string x) < w = error₁ (< (λ ()))
binOpStep (string x) > w = error₁ (> (λ ()))
binOpStep (string x) <= w = error₁ (<= (λ ()))
binOpStep (string x) >= w = error₁ (>= (λ ()))
binOpStep nil ·· y = error₁ (·· (λ ()))
binOpStep (addr x) ·· y = error₁ (BinOpError.·· (λ ()))
binOpStep (number x) ·· y = error₁ (BinOpError.·· (λ ()))
binOpStep (bool x) ·· y = error₁ (BinOpError.·· (λ ()))
binOpStep (string x) ·· nil = error₂ (·· (λ ()))
binOpStep (string x) ·· (addr y) = error₂ (·· (λ ()))
binOpStep (string x) ·· (number y) = error₂ (·· (λ ()))
binOpStep (string x) ·· (bool y) = error₂ (·· (λ ()))
binOpStep (string x) ·· (string y) = step (string (primStringAppend x y)) (·· x y)

data StepResultᴮ {a} (H : Heap a) (B : Block a) : Set
data StepResultᴱ {a} (H : Heap a) (M : Expr a) : Set

data StepResultᴮ H B where
  step : ∀ H′ B′ → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → StepResultᴮ H B
  return : ∀ v {B′} → (B ≡ (return (val v) ∙ B′)) → StepResultᴮ H B
  done : (B ≡ done) → StepResultᴮ H B
  error : (RuntimeErrorᴮ H B) → StepResultᴮ H B

data StepResultᴱ H M where
  step : ∀ H′ M′ → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → StepResultᴱ H M
  value : ∀ V → (M ≡ val V) → StepResultᴱ H M
  error : (RuntimeErrorᴱ H M) → StepResultᴱ H M

stepᴱ : ∀ {a} H M → StepResultᴱ {a} H M
stepᴮ : ∀ {a} H B → StepResultᴮ {a} H B

stepᴱ H (val v) = value v refl
stepᴱ H (var x) = error UnboundVariable
stepᴱ H (M $ N) with stepᴱ H M
stepᴱ H (M $ N) | step H′ M′ D = step H′ (M′ $ N) (app₁ D)
stepᴱ H (_ $ N) | value v refl with stepᴱ H N
stepᴱ H (_ $ N) | value v refl | step H′ N′ s = step H′ (val v $ N′) (app₂ v s)
stepᴱ H (_ $ _) | value (addr a) refl | value w refl with remember (H [ a ])
stepᴱ H (_ $ _) | value (addr a) refl | value w refl  | (nothing , p) = error (app₁ (SEGV p))
stepᴱ H (_ $ _) | value (addr a) refl | value w refl  | (just(function F is B end) , p) = step H (block (fun F) is B [ w / name (arg F) ]ᴮ end) (beta function F is B end w refl p)
stepᴱ H (_ $ _) | value nil refl | value w refl = error (FunctionMismatch nil w (λ ()))
stepᴱ H (_ $ _) | value (number m) refl | value w refl = error (FunctionMismatch (number m) w (λ ()))
stepᴱ H (_ $ _) | value (bool b) refl | value w refl = error (FunctionMismatch (bool b) w (λ ()))
stepᴱ H (_ $ _) | value (string x) refl | value w refl = error (FunctionMismatch (string x) w (λ ()))
stepᴱ H (M $ N) | value V p | error E = error (app₂ E)
stepᴱ H (M $ N) | error E = error (app₁ E)
stepᴱ H (block b is B end) with stepᴮ H B
stepᴱ H (block b is B end) | step H′ B′ D = step H′ (block b is B′ end) (block D)
stepᴱ H (block b is (return _ ∙ B′) end) | return v refl = step H (val v) (return v)
stepᴱ H (block b is done end) | done refl = step H (val nil) done
stepᴱ H (block b is B end) | error E = error (block E)
stepᴱ H (function F is C end) with alloc H (function F is C end)
stepᴱ H function F is C end | ok a H′ p = step H′ (val (addr a)) (function a p)
stepᴱ H (binexp M op N) with stepᴱ H M
stepᴱ H (binexp M op N) | step H′ M′ s = step H′ (binexp M′ op N) (binOp₁ s)
stepᴱ H (binexp M op N) | error E = error (bin₁ E)
stepᴱ H (binexp M op N) | value v refl with stepᴱ H N
stepᴱ H (binexp M op N) | value v refl | step H′ N′ s = step H′ (binexp (val v) op N′) (binOp₂ s)
stepᴱ H (binexp M op N) | value v refl | error E = error (bin₂ E)
stepᴱ H (binexp M op N) | value v refl | value w refl with binOpStep v op w
stepᴱ H (binexp M op N) | value v refl | value w refl | step x p = step H (val x) (binOp₀ p)
stepᴱ H (binexp M op N) | value v refl | value w refl | error₁ E = error (BinOpMismatch₁ v w E)
stepᴱ H (binexp M op N) | value v refl | value w refl | error₂ E = error (BinOpMismatch₂ v w E)

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
 