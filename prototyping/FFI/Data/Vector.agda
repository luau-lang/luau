module FFI.Data.Vector where

open import FFI.Data.Bool using (Bool; false; true)
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
{-# COMPILE GHC empty = \_ -> Data.Vector.empty #-}
{-# COMPILE GHC null = \_ -> Data.Vector.null #-}
{-# COMPILE GHC unsafeHead = \_ -> Data.Vector.unsafeHead #-}
{-# COMPILE GHC unsafeTail = \_ -> Data.Vector.unsafeTail #-}

head : ∀ {A} → (Vector A) → (Maybe A)
head vec with null vec
head vec | false = just (unsafeHead vec)
head vec | true = nothing

tail : ∀ {A} → (Vector A) → Vector A
tail vec with null vec
tail vec | false = unsafeTail vec
tail vec | true = empty

