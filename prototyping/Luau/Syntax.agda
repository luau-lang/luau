module Luau.Syntax where

open import Agda.Builtin.String using (String)

infixr 5 _∙_

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
data Stat : Set
data Expr : Set

data Block where
  _∙_ : Stat → Block → Block
  _∙ : Stat → Block

data Stat where
  function_⟨_⟩_end : Var → Var → Block → Stat
  local_←_ : Var → Expr → Stat
  return : Expr → Stat

data Expr where
  nil : Expr
  var : Var → Expr
  _$_ : Expr → Expr → Expr
