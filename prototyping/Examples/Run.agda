{-# OPTIONS --rewriting #-}
{-# OPTIONS --allow-unsolved-metas #-}

module Examples.Run where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Luau.Syntax using (nil; var; _$_; function_is_end; return; _∙_; done; _⟨_⟩; number; binexp; +)
open import Luau.Value using (nil; number)
open import Luau.Run using (run; return)

ex1 : (run (function "id" ⟨ var "x" ⟩ is return (var "x") ∙ done end ∙ return (var "id" $ nil) ∙ done) ≡ return nil _)
ex1 = {!!}

ex2 : (run (function "fn" ⟨ var "x" ⟩ is return (number 123.0) ∙ done end ∙ return (var "fn" $ nil) ∙ done) ≡ return (number 123.0) _)
ex2 = {!!}

ex3 : (run (function "fn" ⟨ var "x" ⟩ is return (binexp (number 1.0) + (number 2.0)) ∙ done end ∙ return (var "fn" $ nil) ∙ done) ≡ return (number 3.0) _)
ex3 = {!!}
