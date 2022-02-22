{-# OPTIONS --rewriting #-}

module Examples.Run where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Luau.Syntax using (nil; var; _$_; function_is_end; return; _∙_; done; _⟨_⟩)
open import Luau.Value using (nil)
open import Luau.Run using (run; return)

import Agda.Builtin.Equality.Rewrite

ex1 : (run (function "id" ⟨ var "x" ⟩ is return (var "x") ∙ done end ∙ return (var "id" $ nil) ∙ done) ≡ return nil _)
ex1 = refl
