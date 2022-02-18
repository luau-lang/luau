module Properties.Product where

record Σ {A : Set} (B : A → Set) : Set where

  constructor _,_
  field fst : A
  field snd : B fst

open Σ public

_×_ : Set → Set → Set
A × B = Σ (λ (a : A) → B)
