module Luau.RuntimeType where

open import Luau.Value using (Value; nil; addr; number; string)

data RuntimeType : Set where
  function : RuntimeType
  number : RuntimeType
  nil : RuntimeType
  string : RuntimeType

valueType : Value → RuntimeType
valueType nil = nil
valueType (addr x) = function
valueType (number x) = number
valueType (string x) = string
