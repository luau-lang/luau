module Luau.Run where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Luau.Heap using (Heap; emp)
open import Luau.Syntax using (Block; return; _∙_; done)
open import Luau.OpSem using (_⊢_⟶*_⊣_; refl; step)
open import Luau.Value using (val)
open import Properties.Step using (stepᴮ; step; return; done; error)
open import Luau.RuntimeError using (RuntimeErrorᴮ)

data RunResult (H : Heap) (B : Block) : Set where
  return : ∀ V {B′ H′} → (H ⊢ B ⟶* (return (val V) ∙ B′) ⊣ H′) → RunResult H B
  done : ∀ {H′} → (H ⊢ B ⟶* done ⊣ H′) → RunResult H B
  error : ∀ {B′ H′} → (RuntimeErrorᴮ H′ B′) → (H ⊢ B ⟶* B′ ⊣ H′) → RunResult H B

{-# TERMINATING #-}
run′ : ∀ H B → RunResult H B
run′ H B with stepᴮ H B
run′ H B | step H′ B′ D with run′ H′ B′
run′ H B | step H′ B′ D | return V D′  = return V (step D D′)
run′ H B | step H′ B′ D | done D′ = done (step D D′)
run′ H B | step H′ B′ D | error E D′ = error E (step D D′)
run′ H _ | return V refl = return V refl
run′ H _ | done refl = done refl
run′ H B | error E = error E refl

run : ∀ B → RunResult emp B
run = run′ emp
