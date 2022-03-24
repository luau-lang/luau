{-# OPTIONS --rewriting #-}

module Luau.RuntimeError where

open import Agda.Builtin.Equality using (_≡_)
open import Luau.Heap using (Heap; _[_])
open import FFI.Data.Maybe using (just; nothing)
open import FFI.Data.String using (String)
open import Luau.Syntax using (BinaryOperator; Block; Expr; nil; var; val; addr; block_is_end; _$_; local_←_; return; done; _∙_; number; string; binexp; +; -; *; /; <; >; <=; >=; ··)
open import Luau.RuntimeType using (RuntimeType; valueType; function; number; string)
open import Properties.Equality using (_≢_)

data BinOpError : BinaryOperator → RuntimeType → Set where
  + : ∀ {t} → (t ≢ number) → BinOpError + t
  - : ∀ {t} → (t ≢ number) → BinOpError - t
  * : ∀ {t} → (t ≢ number) → BinOpError * t
  / : ∀ {t} → (t ≢ number) → BinOpError / t
  < : ∀ {t} → (t ≢ number) → BinOpError < t
  > : ∀ {t} → (t ≢ number) → BinOpError > t
  <= : ∀ {t} → (t ≢ number) → BinOpError <= t
  >= : ∀ {t} → (t ≢ number) → BinOpError >= t
  ·· : ∀ {t} → (t ≢ string) → BinOpError ·· t
  
data RuntimeErrorᴮ {a} (H : Heap a) : Block a → Set
data RuntimeErrorᴱ {a} (H : Heap a) : Expr a → Set

data RuntimeErrorᴱ H where
  FunctionMismatch : ∀ v w → (valueType v ≢ function) → RuntimeErrorᴱ H (val v $ val w)
  BinOpMismatch₁ : ∀ v w {op} → (BinOpError op (valueType v)) → RuntimeErrorᴱ H (binexp (val v) op (val w))
  BinOpMismatch₂ : ∀ v w {op} → (BinOpError op (valueType w)) → RuntimeErrorᴱ H (binexp (val v) op (val w))
  UnboundVariable : ∀ {x} → RuntimeErrorᴱ H (var x)
  SEGV : ∀ {a} → (H [ a ] ≡ nothing) → RuntimeErrorᴱ H (val (addr a))
  app₁ : ∀ {M N} → RuntimeErrorᴱ H M → RuntimeErrorᴱ H (M $ N)
  app₂ : ∀ {M N} → RuntimeErrorᴱ H N → RuntimeErrorᴱ H (M $ N)
  block : ∀ {b B} → RuntimeErrorᴮ H B → RuntimeErrorᴱ H (block b is B end)
  bin₁ : ∀ {M N op} → RuntimeErrorᴱ H M → RuntimeErrorᴱ H (binexp M op N)
  bin₂ : ∀ {M N op} → RuntimeErrorᴱ H N → RuntimeErrorᴱ H (binexp M op N)

data RuntimeErrorᴮ H where
  local : ∀ {x M B} → RuntimeErrorᴱ H M → RuntimeErrorᴮ H (local x ← M ∙ B)
  return : ∀ {M B} → RuntimeErrorᴱ H M → RuntimeErrorᴮ H (return M ∙ B)
