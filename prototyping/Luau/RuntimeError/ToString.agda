module Luau.RuntimeError.ToString where

open import FFI.Data.String using (String; _++_)
open import Luau.RuntimeError using (RuntimeErrorᴮ; RuntimeErrorᴱ; local; return; NilIsNotAFunction; UnboundVariable; SEGV; app; block)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Var.ToString using (varToString)

errToStringᴱ : ∀ {H B} → RuntimeErrorᴱ H B → String
errToStringᴮ : ∀ {H B} → RuntimeErrorᴮ H B → String

errToStringᴱ NilIsNotAFunction = "nil is not a function"
errToStringᴱ (UnboundVariable x) = "variable " ++ varToString x ++ " is unbound"
errToStringᴱ (SEGV a x) = "address " ++ addrToString a ++ " is unallocated"
errToStringᴱ (app E) = errToStringᴱ E
errToStringᴱ (block b E) = errToStringᴮ E ++ "\n  in call of function " ++ varToString b

errToStringᴮ (local x E) = errToStringᴱ E ++ "\n  in definition of " ++ varToString x 
errToStringᴮ (return E) = errToStringᴱ E ++ "\n  in return statement"
