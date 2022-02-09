module Luau.OpSem where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Maybe using (just)
open import Luau.Heap using (Heap; _≡_⊕_↦_; lookup; function⟨_⟩_end)
open import Luau.Substitution using (_[_/_]ᴮ)
open import Luau.Syntax using (Expr; Stat; Block; nil; addr; var; function⟨_⟩_end; _$_; block_end; local_←_; _∙_; done; function_⟨_⟩_end; return)
open import Luau.Value using (addr; val)

data _⊢_⟶ᴮ_⊣_ : Heap → Block → Block → Heap → Set
data _⊢_⟶ᴱ_⊣_ : Heap → Expr → Expr → Heap → Set

data _⊢_⟶ᴱ_⊣_  where

  nil : ∀ {H} →

    -------------------
    H ⊢ nil ⟶ᴱ nil ⊣ H

  function : ∀ {H H′ a x B} →

    H′ ≡ H ⊕ a ↦ (function⟨ x ⟩ B end) →
    -------------------------------------------
    H ⊢ (function⟨ x ⟩ B end) ⟶ᴱ (addr a) ⊣ H′

  app : ∀ {H H′ M M′ N} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    -----------------------------
    H ⊢ (M $ N) ⟶ᴱ (M′ $ N) ⊣ H′

  beta : ∀ {H M a x B} →
  
    (lookup H a) ≡ just(function⟨ x ⟩ B end) →
    -----------------------------------------------------
    H ⊢ (addr a $ M) ⟶ᴱ (block local x ← M ∙ B end) ⊣ H

  block : ∀ {H H′ B B′} →
 
    H ⊢ B ⟶ᴮ B′ ⊣ H′ →
    ------------------------------------------
    H ⊢ (block B end) ⟶ᴱ (block B′ end) ⊣ H′

  return : ∀ {H V B} →
 
    ---------------------------------------------------
    H ⊢ (block return (val V) ∙ B end) ⟶ᴱ (val V) ⊣ H

  done : ∀ {H} →
 
    ---------------------------------
    H ⊢ (block done end) ⟶ᴱ nil ⊣ H
  
data _⊢_⟶ᴮ_⊣_  where

  local : ∀ {H H′ x M M′ B} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    -------------------------------------------------
    H ⊢ (local x ← M ∙ B) ⟶ᴮ (local x ← M′ ∙ B) ⊣ H′

  subst : ∀ {H x v B} →
  
    -------------------------------------------------
    H ⊢ (local x ← val v ∙ B) ⟶ᴮ (B [ v / x ]ᴮ) ⊣ H

  function : ∀ {H H′ a f x B C} →
  
    H′ ≡ H ⊕ a ↦ (function⟨ x ⟩ C end) →
    --------------------------------------------------------------
    H ⊢ (function f ⟨ x ⟩ C end ∙ B) ⟶ᴮ (B [ addr a / f ]ᴮ) ⊣ H′

  return : ∀ {H H′ M M′ B} →
  
    H ⊢ M ⟶ᴱ M′ ⊣ H′ →
    --------------------------------------------
    H ⊢ (return M ∙ B) ⟶ᴮ (return M′ ∙ B) ⊣ H′

