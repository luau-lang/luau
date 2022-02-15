module FFI.Data.Scientific where

open import FFI.Data.String using (String)
open import FFI.Data.HaskellString using (HaskellString; pack; unpack)

{-# FOREIGN GHC import qualified Data.Scientific #-}
{-# FOREIGN GHC import qualified Text.Show #-}

postulate Scientific : Set
{-# COMPILE GHC Scientific = type Data.Scientific.Scientific #-}

postulate
  showHaskell : Scientific → HaskellString

{-# COMPILE GHC showHaskell = \x -> Text.Show.show x #-}

show : Scientific → String
show x = pack (showHaskell x)
