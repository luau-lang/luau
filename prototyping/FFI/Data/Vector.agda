module FFI.Data.Vector where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Int using (Int; pos; negsuc)
open import Agda.Builtin.Nat using (Nat)
open import FFI.Data.Bool using (Bool; false; true)
open import FFI.Data.HaskellInt using (HaskellInt; haskellIntToInt; intToHaskellInt)
open import FFI.Data.Maybe using (Maybe; just; nothing)

{-# FOREIGN GHC import qualified Data.Vector #-}

postulate Vector : Set → Set
{-# POLARITY Vector ++ #-}
{-# COMPILE GHC Vector = type Data.Vector.Vector #-}

postulate
  empty : ∀ {A} → (Vector A)
  null : ∀ {A} → (Vector A) → Bool
  unsafeHead : ∀ {A} → (Vector A) → A
  unsafeTail : ∀ {A} → (Vector A) → (Vector A)
  length : ∀ {A} → (Vector A) → Nat
  lookup : ∀ {A} → (Vector A) → Nat → (Maybe A)
  snoc : ∀ {A} → (Vector A) → A → (Vector A)
{-# COMPILE GHC empty = \_ -> Data.Vector.empty #-}
{-# COMPILE GHC null = \_ -> Data.Vector.null #-}
{-# COMPILE GHC unsafeHead = \_ -> Data.Vector.unsafeHead #-}
{-# COMPILE GHC unsafeTail = \_ -> Data.Vector.unsafeTail #-}
{-# COMPILE GHC length = \_ -> (fromIntegral . Data.Vector.length) #-}
{-# COMPILE GHC lookup = \_ v -> ((v Data.Vector.!?) . fromIntegral) #-}
{-# COMPILE GHC snoc = \_ -> Data.Vector.snoc #-}

postulate length-empty : ∀ {A} → (length (empty {A}) ≡ 0)
postulate lookup-snoc : ∀ {A} (x : A) (v : Vector A) → (lookup (snoc v x) (length v) ≡ just x)
postulate lookup-snoc-empty : ∀ {A} (x : A) → (lookup (snoc empty x) 0 ≡ just x)

head : ∀ {A} → (Vector A) → (Maybe A)
head vec with null vec
head vec | false = just (unsafeHead vec)
head vec | true = nothing

tail : ∀ {A} → (Vector A) → Vector A
tail vec with null vec
tail vec | false = unsafeTail vec
tail vec | true = empty
