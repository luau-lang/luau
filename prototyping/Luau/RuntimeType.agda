module Luau.RuntimeType where

open import Luau.Syntax using (Value; nil; addr; number; bool; string)

data RuntimeType : Set where
  function : RuntimeType
  number : RuntimeType
  nil : RuntimeType
  boolean : RuntimeType
  string : RuntimeType

valueType : Value → RuntimeType
valueType nil = nil
valueType (addr a) = function
valueType (number n) = number
valueType (bool b) = boolean
valueType (string x) = string
