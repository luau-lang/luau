module FFI.Data.Either where

{-# FOREIGN GHC import qualified Data.Either #-}

data Either (A B : Set) : Set where
  Left : A → Either A B
  Right : B → Either A B
{-# COMPILE GHC Either = data Data.Either.Either (Data.Either.Left|Data.Either.Right) #-}

mapLeft : ∀ {A B C} → (A → B) → (Either A C) → (Either B C)
mapLeft f (Left x) = Left (f x)
mapLeft f (Right x) = Right x

mapRight : ∀ {A B C} → (B → C) → (Either A B) → (Either A C)
mapRight f (Left x) = Left x
mapRight f (Right x) = Right (f x)
