{-# OPTIONS --rewriting #-}

module FFI.Data.Aeson where

open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Equality.Rewrite using ()
open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.String using (String)

open import FFI.Data.ByteString using (ByteString)
open import FFI.Data.HaskellString using (HaskellString; pack)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either; mapL)
open import FFI.Data.Scientific using (Scientific)
open import FFI.Data.Vector using (Vector)

open import Properties.Equality using (_≢_)

{-# FOREIGN GHC import qualified Data.Aeson #-}
{-# FOREIGN GHC import qualified Data.Aeson.Key #-}
{-# FOREIGN GHC import qualified Data.Aeson.KeyMap #-}

postulate
  KeyMap : Set → Set
  Key : Set
  fromString : String → Key
  toString : Key → String
  empty : ∀ {A} → KeyMap A
  singleton : ∀ {A} → Key → A → (KeyMap A)
  insert : ∀ {A} → Key → A → (KeyMap A) → (KeyMap A)
  delete : ∀ {A} → Key → (KeyMap A) → (KeyMap A)
  unionWith : ∀ {A} → (A → A → A) → (KeyMap A) → (KeyMap A) → (KeyMap A)
  lookup : ∀ {A} → Key -> KeyMap A -> Maybe A
{-# POLARITY KeyMap ++ #-}
{-# COMPILE GHC KeyMap = type Data.Aeson.KeyMap.KeyMap #-}
{-# COMPILE GHC Key = type Data.Aeson.Key.Key #-}
{-# COMPILE GHC fromString = Data.Aeson.Key.fromText #-}
{-# COMPILE GHC toString = Data.Aeson.Key.toText #-}
{-# COMPILE GHC empty = \_ -> Data.Aeson.KeyMap.empty #-}
{-# COMPILE GHC singleton = \_ -> Data.Aeson.KeyMap.singleton #-}
{-# COMPILE GHC insert = \_ -> Data.Aeson.KeyMap.insert #-}
{-# COMPILE GHC delete = \_ -> Data.Aeson.KeyMap.delete #-}
{-# COMPILE GHC unionWith = \_ -> Data.Aeson.KeyMap.unionWith #-}
{-# COMPILE GHC lookup = \_ -> Data.Aeson.KeyMap.lookup #-}

postulate lookup-insert : ∀ {A} k v (m : KeyMap A) → (lookup k (insert k v m) ≡ just v)
postulate lookup-empty : ∀ {A} k → (lookup {A} k empty ≡ nothing)
postulate lookup-insert-not : ∀ {A} j k v (m : KeyMap A) → (j ≢ k) → (lookup k m ≡ lookup k (insert j v m))
postulate singleton-insert-empty : ∀ {A} k (v : A) → (singleton k v ≡ insert k v empty)
postulate insert-swap : ∀ {A} j k (v w : A) m → (j ≢ k) → insert j v (insert k w m) ≡ insert k w (insert j v m)
postulate insert-over : ∀ {A} j k (v w : A) m → (j ≡ k) → insert j v (insert k w m) ≡ insert j v m
postulate to-from : ∀ k → toString(fromString k) ≡ k
postulate from-to : ∀ k → fromString(toString k) ≡ k

{-# REWRITE lookup-insert lookup-empty singleton-insert-empty #-}

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
eitherDecode bytes = mapL pack (eitherHDecode bytes)

