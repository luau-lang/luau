module Luau.Heap where

open import FFI.Data.Maybe using (Maybe; just)
open import FFI.Data.Vector using (Vector; length; snoc)
open import Luau.Addr using (Addr)
open import Luau.Value using (Value)

Heap = Vector Value

data _≡_⊕_↦_ : Heap → Heap → Addr → Value → Set where

  defn : ∀ {H val} →

    -----------------------------------
    (snoc H val) ≡ H ⊕ (length H) ↦ val

lookup : Heap → Addr → Maybe Value
lookup = FFI.Data.Vector.lookup

emp : Heap
emp = FFI.Data.Vector.empty
