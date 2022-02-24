module Luau.Value where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Float using (Float)
open import Agda.Builtin.String using (String)
open import Luau.Addr using (Addr)
open import Luau.Syntax using (Block; Expr; nil; addr; number; true; false; string)
open import Luau.Var using (Var)

data Value : Set where
  nil : Value
  addr : Addr → Value
  number : Float → Value
  bool : Bool → Value
  string : String → Value

val : ∀ {a} → Value → Expr a
val nil = nil
val (addr a) = addr a
val (number x) = number x
val (bool false) = false
val (bool true) = true
val (string x) = string x
