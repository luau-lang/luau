module Luau.Syntax where

open import Agda.Builtin.String using (String)

data Type : Set where
  nil : Type
  _⇒_ : Type → Type → Type
  none : Type
  any : Type
  _∪_ : Type → Type → Type
  _∩_ : Type → Type → Type

Var : Set
Var = String

data Block : Set
data Expr : Set

data Block where
  function_⟨_⟩_end_ : Var → Block → Block → Block
  local_←_∙_ : Var → Expr → Block → Block
  return : Expr → Block

data Expr where
  nil : Expr
  var : Var → Expr
  _$_ : Expr → Expr → Expr
