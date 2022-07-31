module Luau.Addr.ToString where

open import Agda.Builtin.String using (String; primStringAppend)
open import Luau.Addr using (Addr)
open import Agda.Builtin.Int using (Int; primShowInteger; pos)

addrToString : Addr → String
addrToString a = primStringAppend "a" (primShowInteger (pos a))
