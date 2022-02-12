module Luau.Value where

open import Luau.Addr using (Addr)
open import Luau.Syntax using (Block; Expr; nil; addr)
open import Luau.Var using (Var)

data Value : Set where
  nil : Value
  addr : Addr → Value

val : ∀ {a} → Value → Expr a
val nil = nil
val (addr a) = addr a


