{-# OPTIONS --rewriting #-}

open import FFI.Data.Either using (Either; Left; Right)
open import Luau.Type using (Type; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_)
open import Luau.TypeNormalization using (normalize)

module Luau.FunctionTypes where

-- The domain of a normalized type
srcⁿ : Type → Type
srcⁿ (S ⇒ T) = S
srcⁿ (S ∩ T) = srcⁿ S ∪ srcⁿ T
srcⁿ never = unknown
srcⁿ T = never

-- To get the domain of a type, we normalize it first We need to do
-- this, since if we try to use it on non-normalized types, we get
--
-- src(number ∩ string) = src(number) ∪ src(string) = never ∪ never
-- src(never) = unknown
--
-- so src doesn't respect type equivalence.
src : Type → Type
src (S ⇒ T) = S
src T = srcⁿ(normalize T)

-- The codomain of a type
tgt : Type → Type
tgt nil = never
tgt (S ⇒ T) = T
tgt never = never
tgt unknown = unknown
tgt number = never
tgt boolean = never
tgt string = never
tgt (S ∪ T) = (tgt S) ∪ (tgt T)
tgt (S ∩ T) = (tgt S) ∩ (tgt T)

