module Luau.AddrCtxt where

open import Luau.Type using (Type)
open import Luau.Addr using (Addr)
open import FFI.Data.Vector using (Vector; empty; lookup)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.VarCtxt using (orBot)

AddrCtxt : Set
AddrCtxt = Vector Type

∅ : AddrCtxt
∅ = empty

_[_] : AddrCtxt → Addr → Type
Σ [ a ] = orBot(lookup Σ a)
