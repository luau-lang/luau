{-# OPTIONS --rewriting #-}

module Luau.Heap where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Maybe using (Maybe; just)
open import FFI.Data.Vector using (Vector; length; snoc; empty)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var)
open import Luau.Syntax using (Block; Expr; Annotated; FunDec; nil; function_is_end)

data HeapValue (a : Annotated) : Set where
  function_is_end : FunDec a → Block a → HeapValue a

Heap : Annotated → Set
Heap a = Vector (HeapValue a)

data _≡_⊕_↦_ {a} : Heap a → Heap a → Addr → HeapValue a → Set where

  defn : ∀ {H val} →

    -----------------------------------
    (snoc H val) ≡ H ⊕ (length H) ↦ val

_[_] : ∀ {a} → Heap a → Addr → Maybe (HeapValue a)
_[_] = FFI.Data.Vector.lookup

∅ : ∀ {a} → Heap a
∅ = empty

data AllocResult a (H : Heap a) (V : HeapValue a) : Set where
  ok : ∀ b H′ → (H′ ≡ H ⊕ b ↦ V) → AllocResult a H V

alloc : ∀ {a} H V → AllocResult a H V
alloc H V = ok (length H) (snoc H V) defn

next : ∀ {a} → Heap a → Addr
next = length

allocated : ∀ {a} → Heap a → HeapValue a → Heap a
allocated = snoc
