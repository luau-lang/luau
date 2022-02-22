module Luau.RuntimeType where

open import Luau.Value using (Value; nil; addr; number)

data RuntimeType : Set where
  function : RuntimeType
  number : RuntimeType
  nil : RuntimeType

valueType : Value → RuntimeType
valueType nil = nil
valueType (addr x) = function
valueType (number x) = number
