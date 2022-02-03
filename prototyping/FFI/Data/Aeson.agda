module FFI.Data.Aeson where

open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.String using (String)

open import FFI.Data.ByteString using (ByteString)
open import FFI.Data.HaskellString using (HaskellString; pack)
open import FFI.Data.Maybe using (Maybe)
open import FFI.Data.Either using (Either; mapLeft)
open import FFI.Data.Scientific using (Scientific)
open import FFI.Data.Vector using (Vector)

{-# FOREIGN GHC import qualified Data.Aeson #-}
{-# FOREIGN GHC import qualified Data.Aeson.Key #-}
{-# FOREIGN GHC import qualified Data.Aeson.KeyMap #-}

postulate
  KeyMap : Set → Set
  Key : Set
  fromString : String → Key
  toString : Key → String
  lookup : ∀ {A} → Key -> KeyMap A -> Maybe A
{-# POLARITY KeyMap ++ #-}
{-# COMPILE GHC KeyMap = type Data.Aeson.KeyMap.KeyMap #-}
{-# COMPILE GHC Key = type Data.Aeson.Key.Key #-}
{-# COMPILE GHC fromString = Data.Aeson.Key.fromText #-}
{-# COMPILE GHC toString = Data.Aeson.Key.toText #-}
{-# COMPILE GHC lookup = \_ -> Data.Aeson.KeyMap.lookup #-}

data Value : Set where
  object : KeyMap Value → Value
  array : Vector Value → Value
  string : String → Value
  number : Scientific → Value
  bool : Bool → Value
  null : Value
{-# COMPILE GHC Value = data Data.Aeson.Value (Data.Aeson.Object|Data.Aeson.Array|Data.Aeson.String|Data.Aeson.Number|Data.Aeson.Bool|Data.Aeson.Null) #-}

Object = KeyMap Value
Array = Vector Value

postulate
  decode : ByteString → Maybe Value
  eitherHDecode : ByteString → Either HaskellString Value
{-# COMPILE GHC decode = Data.Aeson.decodeStrict #-}
{-# COMPILE GHC eitherHDecode = Data.Aeson.eitherDecodeStrict #-}

eitherDecode : ByteString → Either String Value
eitherDecode bytes = mapLeft pack (eitherHDecode bytes)

