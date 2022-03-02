{-# OPTIONS --rewriting #-}

module Examples.OpSem where

open import Luau.OpSem using (_⊢_⟶ᴱ_⊣_; _⊢_⟶ᴮ_⊣_; subst)
open import Luau.Syntax using (Block; var; val; nil; local_←_; _∙_; done; return; block_is_end)
open import Luau.Heap using (∅)

ex1 : ∅ ⊢ (local (var "x") ← val nil ∙ return (var "x") ∙ done) ⟶ᴮ (return (val nil) ∙ done) ⊣ ∅
ex1 = subst nil
