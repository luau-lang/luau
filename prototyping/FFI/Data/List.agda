module FFI.Data.Maybe where

{-# FOREIGN GHC import qualified Data.Maybe #-}

data Maybe (A : Set) : Set where
  Nothing : Maybe A
  Just : A → Maybe A
{-# COMPILE GHC Maybe = data Data.Maybe.Maybe (Data.Maybe.Nothing|Data.Maybe.Just) #-}
