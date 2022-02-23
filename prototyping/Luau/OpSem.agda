module Luau.OpSem where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Float using (Float; primFloatPlus; primFloatMinus; primFloatTimes; primFloatDiv; primFloatEquality; primFloatLess; primFloatInequality)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Utility.Bool using (not; _or_; _and_)
open import Agda.Builtin.Nat using (_==_)
open import FFI.Data.Maybe using (just)
open import Luau.Heap using (Heap; _≡_⊕_↦_; _[_]; function_is_end)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Luau.Syntax using (Expr; Stat; Block; nil; addr; var; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name; fun; arg; binexp; BinaryOperator; +; -; *; /; <; >; ≡; ≅; ≤; ≥; number)
open import Luau.Value using (addr; val; number; Value; bool)
open import Luau.RuntimeType using (RuntimeType; valueType)

evalNumOp : Float → BinaryOperator → Float → Value
evalNumOp x + y = number (primFloatPlus x y)
evalNumOp x - y = number (primFloatMinus x y)
evalNumOp x * y = number (primFloatTimes x y)
evalNumOp x / y = number (primFloatDiv x y)
evalNumOp x < y = bool (primFloatLess x y)
evalNumOp x > y = bool (primFloatLess y x)
evalNumOp x ≡ y = bool (primFloatEquality x y)
evalNumOp x ≅ y = bool (primFloatInequality x y)
evalNumOp x ≤ y = bool ((primFloatLess x y) or (primFloatEquality x y))
evalNumOp x ≥ y = bool ((primFloatLess y x) or (primFloatEquality x y))

evalEqOp : Value → Value → Value
evalEqOp Value.nil Value.nil = bool true
evalEqOp (addr x) (addr y) = bool (x == y)
evalEqOp (number x) (number y) = bool (primFloatEquality x y)
evalEqOp (bool true) (bool y) = bool y
evalEqOp (bool false) (bool y) = bool (not y)
evalEqOp _ _ = bool false

evalNeqOp : Value → Value → Value
evalNeqOp Value.nil Value.nil = bool false
evalNeqOp (addr x) (addr y) = bool (not (x == y))
evalNeqOp (number x) (number y) = bool (primFloatInequality x y)
evalNeqOp (bool true) (bool y) = bool (not y)
evalNeqOp (bool false) (bool y) = bool y
evalNeqOp _ _ = bool true

coerceToBool : Value → Bool
coerceToBool Value.nil = false
coerceToBool (addr x) = true
coerceToBool (number x) = true
coerceToBool (bool x) = x

data _⊢_⟶ᴮ_⊣_ {a} : Heap a → Block a → Block a → Heap a → Set
data _⊢_⟶ᴱ_⊣_ {a} : Heap a → Expr a → Expr a → Heap a → Set

data _⊢_⟶ᴱ_⊣_  where

  nil : ∀ {H} →

    -------------------
    H ⊢ nil ⟶ᴱ nil ⊣ H

  function : ∀ {H H′ a F B} →

    H′ ≡ H ⊕ a ↦ (function F is B end) →
    -------------------------------------------
    H ⊢ (function F is B end) ⟶ᴱ (addr a) ⊣ H′

  app₁ : ∀ {H H′ M M′ N} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    -----------------------------
    H ⊢ (M $ N) ⟶ᴱ (M′ $ N) ⊣ H′

  app₂ : ∀ {H H′ V N N′} →

    H ⊢ N ⟶ᴱ N′ ⊣ H′ →
    -----------------------------
    H ⊢ (val V $ N) ⟶ᴱ (val V $ N′) ⊣ H′

  beta : ∀ {H a F B V} →
  
    H [ a ] ≡ just(function F is B end) →
    -----------------------------------------------------------------------------
    H ⊢ (addr a $ val V) ⟶ᴱ (block (fun F) is (B [ V / name(arg F) ]ᴮ) end) ⊣ H

  block : ∀ {H H′ B B′ b} →
 
    H ⊢ B ⟶ᴮ B′ ⊣ H′ →
    ----------------------------------------------------
    H ⊢ (block b is B end) ⟶ᴱ (block b is B′ end) ⊣ H′

  return : ∀ {H V B b} →
 
    --------------------------------------------------------
    H ⊢ (block b is return (val V) ∙ B end) ⟶ᴱ (val V) ⊣ H

  done : ∀ {H b} →
 
    ---------------------------------
    H ⊢ (block b is done end) ⟶ᴱ nil ⊣ H
  
  binOpEquality :
    ∀ {H x y} →
    ---------------------------------------------------------------------------
    H ⊢ (binexp (val x) BinaryOperator.≡ (val y)) ⟶ᴱ (val (evalEqOp x y)) ⊣ H
  
  binOpInequality :
    ∀ {H x y} →
    ----------------------------------------------------------------------------
    H ⊢ (binexp (val x) BinaryOperator.≅ (val y)) ⟶ᴱ (val (evalNeqOp x y)) ⊣ H
  
  binOpNumbers :
    ∀ {H x op y} →
    -----------------------------------------------------------------------
    H ⊢ (binexp (number x) op (number y)) ⟶ᴱ (val (evalNumOp x op y)) ⊣ H
  
  binOp₁ :
    ∀ {H H′ x x′ op y} →
    H ⊢ x ⟶ᴱ x′ ⊣ H′ →
    ---------------------------------------------
    H ⊢ (binexp x op y) ⟶ᴱ (binexp x′ op y) ⊣ H′
  
  binOp₂ :
    ∀ {H H′ x op y y′} →
    H ⊢ y ⟶ᴱ y′ ⊣ H′ →
    ---------------------------------------------
    H ⊢ (binexp x op y) ⟶ᴱ (binexp x op y′) ⊣ H′

  
data _⊢_⟶ᴮ_⊣_  where

  local : ∀ {H H′ x M M′ B} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    -------------------------------------------------
    H ⊢ (local x ← M ∙ B) ⟶ᴮ (local x ← M′ ∙ B) ⊣ H′

  subst : ∀ {H x v B} →
  
    ------------------------------------------------------
    H ⊢ (local x ← val v ∙ B) ⟶ᴮ (B [ v / name x ]ᴮ) ⊣ H

  function : ∀ {H H′ a F B C} →
  
    H′ ≡ H ⊕ a ↦ (function F is C end) →
    --------------------------------------------------------------
    H ⊢ (function F is C end ∙ B) ⟶ᴮ (B [ addr a / fun F ]ᴮ) ⊣ H′

  return : ∀ {H H′ M M′ B} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    --------------------------------------------
    H ⊢ (return M ∙ B) ⟶ᴮ (return M′ ∙ B) ⊣ H′

data _⊢_⟶*_⊣_ {a} : Heap a → Block a → Block a → Heap a → Set where

  refl : ∀ {H B} →

    ----------------
    H ⊢ B ⟶* B ⊣ H
    
  step : ∀ {H H′ H″ B B′ B″} →
    H ⊢ B ⟶ᴮ B′ ⊣ H′ →
    H′ ⊢ B′ ⟶* B″ ⊣ H″ →
    ------------------
    H ⊢ B ⟶* B″ ⊣ H″
