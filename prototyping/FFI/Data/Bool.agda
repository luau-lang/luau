module FFI.Data.Bool where

{-# FOREIGN GHC import qualified Data.Bool #-}

data Bool : Set where
  false : Bool
  true : Bool
{-# COMPILE GHC Bool = data Data.Bool.Bool (Data.Bool.True|Data.Bool.False) #-}
