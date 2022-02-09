module FFI.Data.HaskellInt where

open import Agda.Builtin.Int using (Int)

{-# FOREIGN GHC import qualified Data.Int #-}

postulate HaskellInt : Set
{-# COMPILE GHC HaskellInt = type Data.Int.Int #-}

postulate
  intToHaskellInt : Int → HaskellInt
  haskellIntToInt : HaskellInt → Int
{-# COMPILE GHC intToHaskellInt = fromIntegral #-}
{-# COMPILE GHC haskellIntToInt = fromIntegral #-}
