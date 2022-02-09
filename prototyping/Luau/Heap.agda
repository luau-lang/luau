module Luau.Heap where

open import FFI.Data.Maybe using (Maybe; just)
open import FFI.Data.Vector using (Vector; length; snoc)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var)
open import Luau.Syntax using (Block; Expr; nil; addr; function⟨_⟩_end)

data HeapValue : Set where
  function⟨_⟩_end : Var → Block → HeapValue

Heap = Vector HeapValue

data _≡_⊕_↦_ : Heap → Heap → Addr → HeapValue → Set where

  defn : ∀ {H val} →

    -----------------------------------
    (snoc H val) ≡ H ⊕ (length H) ↦ val

lookup : Heap → Addr → Maybe HeapValue
lookup = FFI.Data.Vector.lookup

emp : Heap
emp = FFI.Data.Vector.empty

data AllocResult (H : Heap) (V : HeapValue) : Set where
  ok : ∀ a H′ → (H′ ≡ H ⊕ a ↦ V) → AllocResult H V

alloc : ∀ H V → AllocResult H V
alloc H V = ok (length H) (snoc H V) defn
