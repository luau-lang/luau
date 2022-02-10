module Luau.RuntimeError where

open import Agda.Builtin.Equality using (_≡_)
open import Luau.Heap using (Heap; _[_])
open import FFI.Data.Maybe using (just; nothing)
open import Luau.Syntax using (Block; Expr; nil; var; addr; function⟨_⟩_end; block_is_end; _$_; local_←_; function_⟨_⟩_end; return; done; _∙_)

data RuntimeErrorᴮ (H : Heap) : Block → Set
data RuntimeErrorᴱ (H : Heap) : Expr → Set

data RuntimeErrorᴱ H where
  NilIsNotAFunction : ∀ {M} → RuntimeErrorᴱ H (nil $ M)
  UnboundVariable : ∀ x → RuntimeErrorᴱ H (var x)
  SEGV : ∀ a → (H [ a ] ≡ nothing) → RuntimeErrorᴱ H (addr a)
  app : ∀ {M N} → RuntimeErrorᴱ H M → RuntimeErrorᴱ H (M $ N)
  block : ∀ b {B} → RuntimeErrorᴮ H B → RuntimeErrorᴱ H (block b is B end)

data RuntimeErrorᴮ H where
  local : ∀ x {M B} → RuntimeErrorᴱ H M → RuntimeErrorᴮ H (local x ← M ∙ B)
  return : ∀ {M B} → RuntimeErrorᴱ H M → RuntimeErrorᴮ H (return M ∙ B)

