{-# OPTIONS --rewriting #-}

module Examples.Run where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Luau.Syntax using (nil; var; _$_; function_is_end; return; _∙_; done; _⟨_⟩)
open import Luau.Value using (nil)
open import Luau.Run using (run; return)
open import Luau.Heap using (lookup-next; next-emp; lookup-next-emp)

import Agda.Builtin.Equality.Rewrite
{-# REWRITE lookup-next next-emp lookup-next-emp #-}

ex1 : (run (function "id" ⟨ var "x" ⟩ is return (var "x") ∙ done end ∙ return (var "id" $ nil) ∙ done) ≡ return nil _)
ex1 = refl
