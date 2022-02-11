{-# OPTIONS --rewriting #-}

module Examples.Run where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Luau.Syntax using (nil; var; _$_; function_⟨_⟩_end; return; _∙_; done)
open import Luau.Value using (nil)
open import Luau.Run using (run; return)
open import Luau.Heap using (emp; lookup-next; next-emp; lookup-next-emp)

import Agda.Builtin.Equality.Rewrite
{-# REWRITE lookup-next next-emp lookup-next-emp #-}

x = var "x"
id = var "id"

ex1 : (run (function "id" ⟨ "x" ⟩ return x ∙ done end ∙ return (id $ nil) ∙ done) ≡ return nil _)
ex1 = refl
