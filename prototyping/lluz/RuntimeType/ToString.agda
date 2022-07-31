module Luau.RuntimeType.ToString where

open import FFI.Data.String using (String)
open import Luau.RuntimeType using (RuntimeType; function; number; nil; boolean; string)

runtimeTypeToString : RuntimeType → String
runtimeTypeToString function = "function"
runtimeTypeToString number = "number"
runtimeTypeToString nil = "nil"
runtimeTypeToString boolean = "boolean"
runtimeTypeToString string = "string"
