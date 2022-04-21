{-# OPTIONS --rewriting #-}

open import FFI.Data.Either using (Either; Left; Right)
open import Luau.Type using (Type; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src)
open import Properties.Subtyping using (dec-subtyping)

module Luau.OverloadedFunctions where

-- resolve F V is the result of applying a function of type F
-- to an argument of type V. This does function overload resolution,
-- e.g. `resolve (((number) -> string) & ((string) -> number)) (number)` is `string`.

resolve : Type → Type → Type
resolve nil V = never
resolve (S ⇒ T) V = T
resolve never V = never
resolve unknown V = unknown
resolve boolean V = never
resolve number V = never
resolve string V = never
resolve (F ∪ G) V = (resolve F V) ∪ (resolve G V)
resolve (F ∩ G) V with dec-subtyping V (src F) | dec-subtyping V (src G)
resolve (F ∩ G) V | Left p | Left q = resolve F V ∪ resolve G V 
resolve (F ∩ G) V | Right p | Left q = resolve F V
resolve (F ∩ G) V | Left p | Right q = resolve G V
resolve (F ∩ G) V | Right p | Right q = resolve F V ∩ resolve G V
