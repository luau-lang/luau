module Luau.OpSem where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Maybe using (just)
open import Luau.Heap using (Heap; _≡_⊕_↦_; _[_]; function_is_end)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Luau.Syntax using (Expr; Stat; Block; nil; addr; var; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; name; fun; arg)
open import Luau.Value using (addr; val)
open import Luau.Type using (Type)

data _⊢_⟶ᴮ_⊣_ {a} : Heap a → Block a → Block a → Heap a → Set
data _⊢_⟶ᴱ_⊣_ {a} : Heap a → Expr a → Expr a → Heap a → Set

data _⊢_⟶ᴱ_⊣_  where

  function : ∀ {H H′ a F B} →

    H′ ≡ H ⊕ a ↦ (function F is B end) →
    -------------------------------------------
    H ⊢ (function F is B end) ⟶ᴱ (addr a) ⊣ H′

  app : ∀ {H H′ M M′ N} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    -----------------------------
    H ⊢ (M $ N) ⟶ᴱ (M′ $ N) ⊣ H′

  beta : ∀ {H M a F B} →
  
    H [ a ] ≡ just(function F is B end) →
    -----------------------------------------------------
    H ⊢ (addr a $ M) ⟶ᴱ (block (fun F) is local (arg F) ← M ∙ B end) ⊣ H

  block : ∀ {H H′ B B′ b} →
 
    H ⊢ B ⟶ᴮ B′ ⊣ H′ →
    ----------------------------------------------------
    H ⊢ (block b is B end) ⟶ᴱ (block b is B′ end) ⊣ H′

  return : ∀ {H M V B b} →

    M ≡ val V →
    --------------------------------------------
    H ⊢ (block b is return M ∙ B end) ⟶ᴱ M ⊣ H

  done : ∀ {H b} →
 
    ---------------------------------
    H ⊢ (block b is done end) ⟶ᴱ nil ⊣ H
  
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


