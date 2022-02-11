module FFI.Data.Text.Encoding where

open import Agda.Builtin.String using (String)

open import FFI.Data.ByteString using (ByteString)

{-# FOREIGN GHC import qualified Data.Text.Encoding #-}

postulate encodeUtf8 : String → ByteString
{-# COMPILE GHC encodeUtf8 = Data.Text.Encoding.encodeUtf8 #-}
