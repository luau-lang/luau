module FFI.Data.Maybe where

{-# FOREIGN GHC import qualified Data.Maybe #-}

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just : A → Maybe A
{-# COMPILE GHC Maybe = data Data.Maybe.Maybe (Data.Maybe.Nothing|Data.Maybe.Just) #-}
