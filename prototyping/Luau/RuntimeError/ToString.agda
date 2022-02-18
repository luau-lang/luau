module Luau.RuntimeError.ToString where

open import Agda.Builtin.Float using (primShowFloat)
open import FFI.Data.String using (String; _++_)
open import Luau.RuntimeError using (RuntimeErrorᴮ; RuntimeErrorᴱ; local; return; NilIsNotAFunction; NumberIsNotAFunction; UnboundVariable; SEGV; app₁; app₂; block; ValueIsNotANumber)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Syntax.ToString using (exprToString)
open import Luau.Var.ToString using (varToString)
open import Luau.Syntax using (name; _$_)

errToStringᴱ : ∀ {a H B} → RuntimeErrorᴱ {a} H B → String
errToStringᴮ : ∀ {a H B} → RuntimeErrorᴮ {a} H B → String

errToStringᴱ NilIsNotAFunction = "nil is not a function"
errToStringᴱ (NumberIsNotAFunction n) = "number " ++ primShowFloat n ++ " is not a function"
errToStringᴱ (UnboundVariable x) = "variable " ++ varToString x ++ " is unbound"
errToStringᴱ (SEGV a x) = "address " ++ addrToString a ++ " is unallocated"
errToStringᴱ (app₁ E) = errToStringᴱ E
errToStringᴱ (app₂ E) = errToStringᴱ E
errToStringᴱ (block b E) = errToStringᴮ E ++ "\n  in call of function " ++ varToString b
errToStringᴱ (ValueIsNotANumber v) = exprToString v ++ " is not a number"

errToStringᴮ (local x E) = errToStringᴱ E ++ "\n  in definition of " ++ varToString (name x) 
errToStringᴮ (return E) = errToStringᴱ E ++ "\n  in return statement"
 
