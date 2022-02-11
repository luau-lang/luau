module Luau.Heap where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Maybe using (Maybe; just)
open import FFI.Data.Vector using (Vector; length; snoc; empty)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var)
open import Luau.Syntax using (Block; Expr; nil; addr; function⟨_⟩_end)

data HeapValue : Set where
  function_⟨_⟩_end : Var → Var → Block → HeapValue

Heap = Vector HeapValue

data _≡_⊕_↦_ : Heap → Heap → Addr → HeapValue → Set where

  defn : ∀ {H val} →

    -----------------------------------
    (snoc H val) ≡ H ⊕ (length H) ↦ val

lookup : Heap → Addr → Maybe HeapValue
lookup = FFI.Data.Vector.lookup

emp : Heap
emp = empty

data AllocResult (H : Heap) (V : HeapValue) : Set where
  ok : ∀ a H′ → (H′ ≡ H ⊕ a ↦ V) → AllocResult H V

alloc : ∀ H V → AllocResult H V
alloc H V = ok (length H) (snoc H V) defn

next : Heap → Addr
next = length

allocated : Heap → HeapValue → Heap
allocated = snoc

-- next-emp : (length empty ≡ 0)
next-emp = FFI.Data.Vector.length-empty

-- lookup-next : ∀ V H → (lookup (allocated H V) (next H) ≡ just V)
lookup-next = FFI.Data.Vector.lookup-snoc

-- lookup-next-emp : ∀ V → (lookup (allocated emp V) 0 ≡ just V)
lookup-next-emp = FFI.Data.Vector.lookup-snoc-empty

