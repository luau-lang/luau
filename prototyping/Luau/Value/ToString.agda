module Luau.Value.ToString where

open import Agda.Builtin.String using (String; primShowString)
open import Agda.Builtin.Float using (primShowFloat)
open import Agda.Builtin.Bool using (true; false)
open import Luau.Value using (Value; nil; addr; number; bool; string)
open import Luau.Addr.ToString using (addrToString)

valueToString : Value → String
valueToString nil = "nil"
valueToString (addr a) = addrToString a
valueToString (number x) = primShowFloat x
valueToString (bool false) = "false"
valueToString (bool true) = "true"
valueToString (string x) = primShowString x
