{-# OPTIONS --rewriting #-}

module Luau.OpSem where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Float using (Float; primFloatPlus; primFloatMinus; primFloatTimes; primFloatDiv; primFloatEquality; primFloatLess; primFloatInequality)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.String using (primStringEquality; primStringAppend)
open import Utility.Bool using (not; _or_; _and_)
open import Agda.Builtin.Nat using () renaming (_==_ to _==ᴬ_)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; _≡_⊕_↦_; _[_]; function_is_end)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Luau.Syntax using (Value; Expr; Stat; Block; nil; addr; val; var; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name; fun; arg; binexp; BinaryOperator; +; -; *; /; <; >; ==; ~=; <=; >=; ··; number; bool; string)
open import Luau.RuntimeType using (RuntimeType; valueType)
open import Properties.Product using (_×_; _,_)

evalEqOp : Value → Value → Bool
evalEqOp Value.nil Value.nil = true
evalEqOp (addr x) (addr y) = (x ==ᴬ y)
evalEqOp (number x) (number y) = primFloatEquality x y
evalEqOp (bool true) (bool y) = y
evalEqOp (bool false) (bool y) = not y
evalEqOp _ _ = false

evalNeqOp : Value → Value → Bool
evalNeqOp (number x) (number y) = primFloatInequality x y
evalNeqOp x y = not (evalEqOp x y)

data _⟦_⟧_⟶_ : Value → BinaryOperator → Value → Value → Set where
  + : ∀ m n → (number m) ⟦ + ⟧ (number n) ⟶ number (primFloatPlus m n)
  - : ∀ m n → (number m) ⟦ - ⟧ (number n) ⟶ number (primFloatMinus m n)
  / : ∀ m n → (number m) ⟦ / ⟧ (number n) ⟶ number (primFloatTimes m n)
  * : ∀ m n → (number m) ⟦ * ⟧ (number n) ⟶ number (primFloatDiv m n)
  < : ∀ m n → (number m) ⟦ < ⟧ (number n) ⟶ bool (primFloatLess m n)
  > : ∀ m n → (number m) ⟦ > ⟧ (number n) ⟶ bool (primFloatLess n m)
  <= : ∀ m n → (number m) ⟦ <= ⟧ (number n) ⟶ bool ((primFloatLess m n) or (primFloatEquality m n))
  >= : ∀ m n → (number m) ⟦ >= ⟧ (number n) ⟶ bool ((primFloatLess n m) or (primFloatEquality m n))
  == : ∀ v w → v ⟦ == ⟧ w ⟶ bool (evalEqOp v w)
  ~= : ∀ v w → v ⟦ ~= ⟧ w ⟶ bool (evalNeqOp v w)
  ·· : ∀ x y → (string x) ⟦ ·· ⟧ (string y) ⟶ string (primStringAppend x y)

data _⊢_⟶ᴮ_⊣_ {a} : Heap a → Block a → Block a → Heap a → Set
data _⊢_⟶ᴱ_⊣_ {a} : Heap a → Expr a → Expr a → Heap a → Set

data _⊢_⟶ᴱ_⊣_  where

  function : ∀ a {H H′ F B} →

    H′ ≡ H ⊕ a ↦ (function F is B end) →
    -------------------------------------------
    H ⊢ (function F is B end) ⟶ᴱ val(addr a) ⊣ H′

  app₁ : ∀ {H H′ M M′ N} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    -----------------------------
    H ⊢ (M $ N) ⟶ᴱ (M′ $ N) ⊣ H′

  app₂ : ∀ v {H H′ N N′} →

    H ⊢ N ⟶ᴱ N′ ⊣ H′ →
    -----------------------------
    H ⊢ (val v $ N) ⟶ᴱ (val v $ N′) ⊣ H′

  beta : ∀ O v {H a F B} →

    (O ≡ function F is B end) →
    H [ a ] ≡ just(O) →
    -----------------------------------------------------------------------------
    H ⊢ (val (addr a) $ val v) ⟶ᴱ (block (fun F) is (B [ v / name(arg F) ]ᴮ) end) ⊣ H

  block : ∀ {H H′ B B′ b} →
 
    H ⊢ B ⟶ᴮ B′ ⊣ H′ →
    ----------------------------------------------------
    H ⊢ (block b is B end) ⟶ᴱ (block b is B′ end) ⊣ H′

  return : ∀ v {H B b} →

    --------------------------------------------------------
    H ⊢ (block b is return (val v) ∙ B end) ⟶ᴱ val v ⊣ H

  done : ∀ {H b} →
 
    --------------------------------------------
    H ⊢ (block b is done end) ⟶ᴱ (val nil) ⊣ H
  
  binOp₀ : ∀ {H op v₁ v₂ w} →

    v₁ ⟦ op ⟧ v₂ ⟶ w → 
    --------------------------------------------------
    H ⊢ (binexp (val v₁) op (val v₂)) ⟶ᴱ (val w) ⊣ H

  binOp₁ : ∀ {H H′ x x′ op y} →

    H ⊢ x ⟶ᴱ x′ ⊣ H′ →
    ---------------------------------------------
    H ⊢ (binexp x op y) ⟶ᴱ (binexp x′ op y) ⊣ H′
  
  binOp₂ : ∀ {H H′ x op y y′} →

    H ⊢ y ⟶ᴱ y′ ⊣ H′ →
    ---------------------------------------------
    H ⊢ (binexp x op y) ⟶ᴱ (binexp x op y′) ⊣ H′

data _⊢_⟶ᴮ_⊣_  where

  local : ∀ {H H′ x M M′ B} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    -------------------------------------------------
    H ⊢ (local x ← M ∙ B) ⟶ᴮ (local x ← M′ ∙ B) ⊣ H′

  subst : ∀ v {H x B} →
  
    ------------------------------------------------------
    H ⊢ (local x ← val v ∙ B) ⟶ᴮ (B [ v / name x ]ᴮ) ⊣ H

  function : ∀ a {H H′ F B C} →
  
    H′ ≡ H ⊕ a ↦ (function F is C end) →
    --------------------------------------------------------------
    H ⊢ (function F is C end ∙ B) ⟶ᴮ (B [ addr a / name(fun F) ]ᴮ) ⊣ H′

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
