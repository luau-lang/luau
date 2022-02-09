module Luau.Var.ToString where

open import Agda.Builtin.String using (String)
open import Luau.Var using (Var)

varToString : Var → String
varToString x = x

