module Luau.Type where

data Type : Set where
  nil : Type
  _⇒_ : Type → Type → Type
  none : Type
  any : Type
  _∪_ : Type → Type → Type
  _∩_ : Type → Type → Type

