module FFI.Data.Aeson where

open import Agda.Builtin.String using (String)

open import FFI.Data.ByteString using (ByteString)
open import FFI.Data.HaskellString using (HaskellString)
open import FFI.Data.Maybe using (Maybe)
open import FFI.Data.Either using (Either)

{-# FOREIGN GHC import qualified Data.Aeson #-}

postulate Value : Set
{-# COMPILE GHC Value = type Data.Aeson.Value #-}

postulate decode : ByteString → Maybe Value
{-# COMPILE GHC decode = Data.Aeson.decodeStrict #-}

postulate eitherDecode : ByteString → Either HaskellString Value
{-# COMPILE GHC eitherDecode = Data.Aeson.eitherDecodeStrict #-}
