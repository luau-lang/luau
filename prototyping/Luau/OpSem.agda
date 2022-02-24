{-# OPTIONS --rewriting #-}

module Luau.OpSem where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Float using (Float; primFloatPlus; primFloatMinus; primFloatTimes; primFloatDiv)
open import FFI.Data.Maybe using (just)
open import Luau.Heap using (Heap; _≡_⊕_↦_; _[_]; function_is_end)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Luau.Syntax using (Expr; Stat; Block; nil; addr; var; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name; fun; arg; binexp; BinaryOperator; +; -; *; /; number)
open import Luau.Value using (addr; val; number)

evalBinOp : Float → BinaryOperator → Float → Float
evalBinOp x + y = primFloatPlus x y
evalBinOp x - y = primFloatMinus x y
evalBinOp x * y = primFloatTimes x y
evalBinOp x / y = primFloatDiv x y

data _⊢_⟶ᴮ_⊣_ {a} : Heap a → Block a → Block a → Heap a → Set
data _⊢_⟶ᴱ_⊣_ {a} : Heap a → Expr a → Expr a → Heap a → Set

data _⊢_⟶ᴱ_⊣_  where

  function : ∀ a {H H′ F B} →

    H′ ≡ H ⊕ a ↦ (function F is B end) →
    -------------------------------------------
    H ⊢ (function F is B end) ⟶ᴱ (addr a) ⊣ H′

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
    H ⊢ (addr a $ val v) ⟶ᴱ (block (fun F) is (B [ v / name(arg F) ]ᴮ) end) ⊣ H

  block : ∀ {H H′ B B′ b} →
 
    H ⊢ B ⟶ᴮ B′ ⊣ H′ →
    ----------------------------------------------------
    H ⊢ (block b is B end) ⟶ᴱ (block b is B′ end) ⊣ H′

  return : ∀ v {H B b} →

    --------------------------------------------------------
    H ⊢ (block b is return (val v) ∙ B end) ⟶ᴱ val v ⊣ H

  done : ∀ {H b} →
 
    ---------------------------------
    H ⊢ (block b is done end) ⟶ᴱ nil ⊣ H
  
  binOpEval : ∀ {H op} m n →

    --------------------------------------------------------------------------
    H ⊢ (binexp (number m) op (number n)) ⟶ᴱ (number (evalBinOp m op n)) ⊣ H
  
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
