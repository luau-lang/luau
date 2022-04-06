{-# OPTIONS --rewriting #-}

open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_)
open import Properties.Equality using (_≢_)

module Luau.Subtyping where

-- An implementation of semantic subtyping

-- We think of types as languages of trees

data Tree : Set where

  scalar : ∀ {T} → Scalar T → Tree
  function : Tree
  function-ok : Tree → Tree
  function-err : Tree → Tree

data Language : Type → Tree → Set
data ¬Language : Type → Tree → Set

data Language where

  scalar : ∀ {T} → (s : Scalar T) → Language T (scalar s)
  function : ∀ {T U} → Language (T ⇒ U) function
  function-ok : ∀ {T U u} → (Language U u) → Language (T ⇒ U) (function-ok u)
  function-err : ∀ {T U t} → (¬Language T t) → Language (T ⇒ U) (function-err t)
  scalar-function-err : ∀ {S t} → (Scalar S) → Language S (function-err t)
  left : ∀ {T U t} → Language T t → Language (T ∪ U) t
  right : ∀ {T U u} → Language U u → Language (T ∪ U) u
  _,_ : ∀ {T U t} → Language T t → Language U t → Language (T ∩ U) t
  unknown : ∀ {t} → Language unknown t

data ¬Language where

  scalar-scalar : ∀ {S T} → (s : Scalar S) → (Scalar T) → (S ≢ T) → ¬Language T (scalar s)
  scalar-function : ∀ {S} → (Scalar S) → ¬Language S function
  scalar-function-ok : ∀ {S u} → (Scalar S) → ¬Language S (function-ok u)
  function-scalar : ∀ {S T U} (s : Scalar S) → ¬Language (T ⇒ U) (scalar s)
  function-ok : ∀ {T U u} → (¬Language U u) → ¬Language (T ⇒ U) (function-ok u)
  function-err : ∀ {T U t} → (Language T t) → ¬Language (T ⇒ U) (function-err t)
  _,_ : ∀ {T U t} → ¬Language T t → ¬Language U t → ¬Language (T ∪ U) t
  left : ∀ {T U t} → ¬Language T t → ¬Language (T ∩ U) t
  right : ∀ {T U u} → ¬Language U u → ¬Language (T ∩ U) u
  never : ∀ {t} → ¬Language never t

-- Subtyping as language inclusion

_<:_ : Type → Type → Set
(T <: U) = ∀ t → (Language T t) → (Language U t)

-- For warnings, we are interested in failures of subtyping,
-- which is whrn there is a tree in T's language that isn't in U's.

data _≮:_ (T U : Type) : Set where

  witness : ∀ t →

    Language T t →
    ¬Language U t →
    -----------------
    T ≮: U
