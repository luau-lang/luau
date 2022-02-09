module Luau.Value.ToString where

open import Agda.Builtin.String using (String)
open import Luau.Value using (Value; nil; addr)
open import Luau.Addr.ToString using (addrToString)

valueToString : Value → String
valueToString nil = "nil"
valueToString (addr a) = addrToString a
 
