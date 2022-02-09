module Luau.Value where

open import Luau.Addr using (Addr)
open import Luau.Syntax using (Block; Expr; nil; addr; function⟨_⟩_end)
open import Luau.Var using (Var)

data Value : Set where
  nil : Value
  addr : Addr → Value

val : Value → Expr
val nil = nil
val (addr a) = addr a


