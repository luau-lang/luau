module Luau.Syntax where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Float using (Float)
open import Agda.Builtin.String using (String)
open import Luau.Var using (Var)
open import Luau.Addr using (Addr)
open import Luau.Type using (Type)
open import FFI.Data.Maybe using (Maybe; just; nothing)

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

fun : ∀ {a} → FunDec a → VarDec a
fun (f ⟨ x ⟩∈ T) = (var f ∈ T)
fun (f ⟨ x ⟩) = (var f)

arg : ∀ {a} → FunDec a → VarDec a
arg (f ⟨ x ⟩∈ T) = x
arg (f ⟨ x ⟩) = x

data BinaryOperator : Set where
  + : BinaryOperator
  - : BinaryOperator
  * : BinaryOperator
  / : BinaryOperator
  < : BinaryOperator
  > : BinaryOperator
  == : BinaryOperator
  ~= : BinaryOperator
  <= : BinaryOperator
  >= : BinaryOperator
  ·· : BinaryOperator

data Value : Set where
  nil : Value
  addr : Addr → Value
  number : Float → Value
  bool : Bool → Value
  string : String → Value

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
  var : Var → Expr a
  val : Value → Expr a
  _$_ : Expr a → Expr a → Expr a
  function_is_end : FunDec a → Block a → Expr a
  block_is_end : VarDec a → Block a → Expr a
  binexp : Expr a → BinaryOperator → Expr a → Expr a

isAnnotatedᴱ : ∀ {a} → Expr a → Maybe (Expr yes)
isAnnotatedᴮ : ∀ {a} → Block a → Maybe (Block yes)

isAnnotatedᴱ (var x) = just (var x)
isAnnotatedᴱ (val v) = just (val v)
isAnnotatedᴱ (M $ N) with isAnnotatedᴱ M | isAnnotatedᴱ N
isAnnotatedᴱ (M $ N) | just M′ | just N′ = just (M′ $ N′)
isAnnotatedᴱ (M $ N) | _ | _ = nothing
isAnnotatedᴱ (function f ⟨ var x ∈ T ⟩∈ U is B end) with isAnnotatedᴮ B
isAnnotatedᴱ (function f ⟨ var x ∈ T ⟩∈ U is B end) | just B′ = just (function f ⟨ var x ∈ T ⟩∈ U is B′ end)
isAnnotatedᴱ (function f ⟨ var x ∈ T ⟩∈ U is B end) | _ = nothing
isAnnotatedᴱ (function _ is B end) = nothing
isAnnotatedᴱ (block var b ∈ T is B end) with isAnnotatedᴮ B
isAnnotatedᴱ (block var b ∈ T is B end) | just B′ = just (block var b ∈ T is B′ end)
isAnnotatedᴱ (block var b ∈ T is B end) | _ = nothing
isAnnotatedᴱ (block _ is B end) = nothing
isAnnotatedᴱ (binexp M op N) with isAnnotatedᴱ M | isAnnotatedᴱ N
isAnnotatedᴱ (binexp M op N) | just M′ | just N′ = just (binexp M′ op N′)
isAnnotatedᴱ (binexp M op N) | _ | _ = nothing

isAnnotatedᴮ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) with isAnnotatedᴮ B | isAnnotatedᴮ C
isAnnotatedᴮ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) | just B′ | just C′ = just (function f ⟨ var x ∈ T ⟩∈ U is C′ end ∙ B′)
isAnnotatedᴮ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) | _ | _ = nothing
isAnnotatedᴮ (function _ is C end ∙ B) = nothing
isAnnotatedᴮ (local var x ∈ T ← M ∙ B) with isAnnotatedᴱ M | isAnnotatedᴮ B
isAnnotatedᴮ (local var x ∈ T ← M ∙ B) | just M′ | just B′ = just (local var x ∈ T ← M′ ∙ B′)
isAnnotatedᴮ (local var x ∈ T ← M ∙ B) | _ | _ = nothing
isAnnotatedᴮ (local _ ← M ∙ B) = nothing
isAnnotatedᴮ (return M ∙ B) with isAnnotatedᴱ M | isAnnotatedᴮ B
isAnnotatedᴮ (return M ∙ B) | just M′ | just B′ = just (return M′ ∙ B′)
isAnnotatedᴮ (return M ∙ B) | _ | _ = nothing
isAnnotatedᴮ done = just done
