module Luau.RuntimeError.ToString where

open import Agda.Builtin.Float using (primShowFloat)
open import FFI.Data.String using (String; _++_)
open import Luau.RuntimeError using (RuntimeErrorᴮ; RuntimeErrorᴱ; local; return; TypeMismatch; UnboundVariable; SEGV; app₁; app₂; block; bin₁; bin₂)
open import Luau.RuntimeType.ToString using (runtimeTypeToString)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Syntax.ToString using (exprToString)
open import Luau.Var.ToString using (varToString)
open import Luau.Value.ToString using (valueToString)
open import Luau.Syntax using (name; _$_)

errToStringᴱ : ∀ {a H B} → RuntimeErrorᴱ {a} H B → String
errToStringᴮ : ∀ {a H B} → RuntimeErrorᴮ {a} H B → String

errToStringᴱ (UnboundVariable x) = "variable " ++ varToString x ++ " is unbound"
errToStringᴱ (SEGV a x) = "address " ++ addrToString a ++ " is unallocated"
errToStringᴱ (app₁ E) = errToStringᴱ E
errToStringᴱ (app₂ E) = errToStringᴱ E
errToStringᴱ (bin₁ E) = errToStringᴱ E
errToStringᴱ (bin₂ E) = errToStringᴱ E
errToStringᴱ (block b E) = errToStringᴮ E ++ "\n  in call of function " ++ varToString b
errToStringᴱ (TypeMismatch t v _) = "value " ++ valueToString v ++ " is not a " ++ runtimeTypeToString t

errToStringᴮ (local x E) = errToStringᴱ E ++ "\n  in definition of " ++ varToString (name x) 
errToStringᴮ (return E) = errToStringᴱ E ++ "\n  in return statement"
 
