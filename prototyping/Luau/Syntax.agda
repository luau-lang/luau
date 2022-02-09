module Luau.Syntax where

open import Luau.Var using (Var)
open import Luau.Addr using (Addr)

infixr 5 _∙_

data Block : Set
data Stat : Set
data Expr : Set

data Block where
  _∙_ : Stat → Block → Block
  done : Block

data Stat where
  function_⟨_⟩_end : Var → Var → Block → Stat
  local_←_ : Var → Expr → Stat
  return : Expr → Stat

data Expr where
  nil : Expr
  var : Var → Expr
  addr : Addr → Expr
  _$_ : Expr → Expr → Expr
  function⟨_⟩_end : Var → Block → Expr
  block_end : Block → Expr
