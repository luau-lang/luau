module FFI.Data.Maybe where

open import Agda.Builtin.Equality using (_≡_; refl)

{-# FOREIGN GHC import qualified Data.Maybe #-}

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just : A → Maybe A
{-# COMPILE GHC Maybe = data Data.Maybe.Maybe (Data.Maybe.Nothing|Data.Maybe.Just) #-}

just-inv : ∀ {A} {x y : A} → (just x ≡ just y) → (x ≡ y)
just-inv refl = refl

