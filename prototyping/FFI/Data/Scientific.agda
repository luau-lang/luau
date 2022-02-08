module FFI.Data.Scientific where

{-# FOREIGN GHC import qualified Data.Scientific #-}

postulate Scientific : Set
{-# COMPILE GHC Scientific = type Data.Scientific.Scientific #-}
