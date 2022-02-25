module Luau.RuntimeType where

open import Luau.Syntax using (Value; nil; addr; number; bool)

data RuntimeType : Set where
  function : RuntimeType
  number : RuntimeType
  nil : RuntimeType
  boolean : RuntimeType

valueType : Value → RuntimeType
valueType nil = nil
valueType (addr a) = function
valueType (number n) = number
valueType (bool b) = boolean
