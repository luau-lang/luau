{-# OPTIONS --rewriting #-}

module Luau.Heap where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Vector using (Vector; length; snoc; empty; lookup-snoc-not)
open import Luau.Addr using (Addr; _≡ᴬ_)
open import Luau.Var using (Var)
open import Luau.Syntax using (Block; Expr; Annotated; FunDec; nil; function_is_end)
open import Properties.Equality using (_≢_; trans)
open import Properties.Remember using (remember; _,_)
open import Properties.Dec using (yes; no)

-- Heap-allocated objects
data Object (a : Annotated) : Set where

  function_is_end : FunDec a → Block a → Object a

Heap : Annotated → Set
Heap a = Vector (Object a)

data _≡_⊕_↦_ {a} : Heap a → Heap a → Addr → Object a → Set where

  defn : ∀ {H val} →

    -----------------------------------
    (snoc H val) ≡ H ⊕ (length H) ↦ val

_[_] : ∀ {a} → Heap a → Addr → Maybe (Object a)
_[_] = FFI.Data.Vector.lookup

∅ : ∀ {a} → Heap a
∅ = empty

data AllocResult a (H : Heap a) (V : Object a) : Set where
  ok : ∀ b H′ → (H′ ≡ H ⊕ b ↦ V) → AllocResult a H V

alloc : ∀ {a} H V → AllocResult a H V
alloc H V = ok (length H) (snoc H V) defn

next : ∀ {a} → Heap a → Addr
next = length

allocated : ∀ {a} → Heap a → Object a → Heap a
allocated = snoc

lookup-not-allocated : ∀ {a} {H H′ : Heap a} {b c O} → (H′ ≡ H ⊕ b ↦ O) → (c ≢ b) → (H [ c ] ≡ H′ [ c ])
lookup-not-allocated {H = H} {O = O} defn p = lookup-snoc-not O H p
