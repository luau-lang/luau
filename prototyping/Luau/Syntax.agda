module Luau.Syntax where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Float using (Float)
open import Properties.Dec using (⊥)
open import Luau.Var using (Var)
open import Luau.Addr using (Addr)
open import Luau.Type using (Type)

infixr 5 _∙_

data Annotated : Set where
  maybe : Annotated
  yes : Annotated

data VarDec : Annotated → Set where
  var : Var → VarDec maybe
  var_∈_ : ∀ {a} → Var → Type → VarDec a

name : ∀ {a} → VarDec a → Var
name (var x) = x
name (var x ∈ T) = x

data FunDec : Annotated → Set where
  _⟨_⟩∈_ : ∀ {a} → Var → VarDec a → Type → FunDec a
  _⟨_⟩ : Var → VarDec maybe → FunDec maybe

fun : ∀ {a} → FunDec a → Var
fun (f ⟨ x ⟩∈ T) = f
fun (f ⟨ x ⟩) = f

arg : ∀ {a} → FunDec a → VarDec a
arg (f ⟨ x ⟩∈ T) = x
arg (f ⟨ x ⟩) = x

data Block (a : Annotated) : Set
data Stat (a : Annotated) : Set
data Expr (a : Annotated) : Set

data Block a where
  _∙_ : Stat a → Block a → Block a
  done : Block a

data Stat a where
  function_is_end : FunDec a → Block a → Stat a
  local_←_ : VarDec a → Expr a → Stat a
  return : Expr a → Stat a

data Expr a where
  nil : Expr a
  var : Var → Expr a
  addr : Addr → Expr a
  _$_ : Expr a → Expr a → Expr a
  function_is_end : FunDec a → Block a → Expr a
  block_is_end : Var → Block a → Expr a
  number : Float → Expr a
