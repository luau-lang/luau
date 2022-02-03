module FFI.Data.Either where

{-# FOREIGN GHC import qualified Data.Either #-}

data Either (A B : Set) : Set where
  Left : A → Either A B
  Right : B → Either A B
{-# COMPILE GHC Either = data Data.Either.Either (Data.Either.Left|Data.Either.Right) #-}
