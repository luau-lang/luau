module Luau.Syntax where

open import Luau.Var using (Var)
open import Luau.Addr using (Addr)
open import Luau.Type using (Type)

infixr 5 _∙_

data VarDec : Set where
  var : Var → VarDec
  var_∈_ : Var → Type → VarDec

name : VarDec → Var
name (var x) = x
name (var x ∈ T) = x

data Block : Set
data Stat : Set
data Expr : Set

data Block where
  _∙_ : Stat → Block → Block
  done : Block

data Stat where
  function_⟨_⟩_end : Var → VarDec → Block → Stat
  local_←_ : VarDec → Expr → Stat
  return : Expr → Stat

data Expr where
  nil : Expr
  var : Var → Expr
  addr : Addr → Expr
  _$_ : Expr → Expr → Expr
  function⟨_⟩_end : VarDec → Block → Expr
  block_is_end : Var → Block → Expr

