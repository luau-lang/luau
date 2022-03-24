module FFI.Data.Either where

{-# FOREIGN GHC import qualified Data.Either #-}

data Either (A B : Set) : Set where
  Left : A → Either A B
  Right : B → Either A B
{-# COMPILE GHC Either = data Data.Either.Either (Data.Either.Left|Data.Either.Right) #-}

swapLR : ∀ {A B} → Either A B → Either B A
swapLR (Left x) = Right x
swapLR (Right x) = Left x

mapL : ∀ {A B C} → (A → B) → Either A C → Either B C
mapL f (Left x) = Left (f x)
mapL f (Right x) = Right x

mapR : ∀ {A B C} → (B → C) → Either A B → Either A C
mapR f (Left x) = Left x
mapR f (Right x) = Right (f x)

mapLR : ∀ {A B C D} → (A → B) → (C → D) → Either A C → Either B D
mapLR f g (Left x) = Left (f x)
mapLR f g (Right x) = Right (g x)

cond : ∀ {A B C : Set} → (A → C) → (B → C) → (Either A B) → C
cond f g (Left x) = f x
cond f g (Right x) = g x
