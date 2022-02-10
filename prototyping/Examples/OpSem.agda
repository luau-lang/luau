module Examples.OpSem where

open import Luau.OpSem using (_⊢_⟶ᴱ_⊣_; _⊢_⟶ᴮ_⊣_; subst)
open import Luau.Syntax using (var; nil; local_←_; _∙_; done; return; block_is_end)
open import Luau.Heap using (emp)

ex1 : emp ⊢ (local (var "x") ← nil ∙ return (var "x") ∙ done) ⟶ᴮ (return nil ∙ done) ⊣ emp
ex1 = subst

