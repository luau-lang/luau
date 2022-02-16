module Luau.Value.ToString where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.Float using (primShowFloat)
open import Luau.Value using (Value; nil; addr; number)
open import Luau.Addr.ToString using (addrToString)

valueToString : Value → String
valueToString nil = "nil"
valueToString (addr a) = addrToString a
valueToString (number x) = primShowFloat x
