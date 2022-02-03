module FFI.Data.HaskellString where

open import Agda.Builtin.String using (String)

{-# FOREIGN GHC import qualified Data.String #-}
{-# FOREIGN GHC import qualified Data.Text #-}

postulate HaskellString : Set
{-# COMPILE GHC HaskellString = type Data.String.String #-}

postulate pack : HaskellString → String
{-# COMPILE GHC pack = Data.Text.pack #-}

postulate unpack : String → HaskellString
{-# COMPILE GHC unpack = Data.Text.unpack #-}

