module FFI.Data.Vector where

{-# FOREIGN GHC import qualified Data.Vector #-}

postulate Vector : Set → Set
{-# POLARITY Vector ++ #-}
{-# COMPILE GHC Vector = type Data.Vector.Vector #-}
