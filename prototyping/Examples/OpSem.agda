module Examples.OpSem where

open import Luau.OpSem using (_⊢_⟶ᴱ_⊣_; _⊢_⟶ᴮ_⊣_; subst)
open import Luau.Syntax using (var; nil; local_←_; _∙_; done; return)
open import Luau.Heap using (emp)

x = var "x"

ex1 : emp ⊢ (local "x" ← nil ∙ return x ∙ done) ⟶ᴮ (return nil ∙ done) ⊣ emp
ex1 = subst

