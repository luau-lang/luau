module Luau.RuntimeError.ToString where

open import FFI.Data.String using (String; _++_)
open import Luau.RuntimeError using (RuntimeErrorᴮ; RuntimeErrorᴱ; local; return; ValueNotCallable; UnboundVariable; SEGV; app; block)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Syntax.ToString using (exprToString)
open import Luau.Var.ToString using (varToString)
open import Luau.Syntax using (name; _$_)

errToStringᴱ : ∀ {a H B} → RuntimeErrorᴱ {a} H B → String
errToStringᴮ : ∀ {a H B} → RuntimeErrorᴮ {a} H B → String

errToStringᴱ (ValueNotCallable (x $ _)) = "value " ++ exprToString x ++ " is not callable"
errToStringᴱ (ValueNotCallable x) = "value " ++ exprToString x ++ " is not callable"
errToStringᴱ (UnboundVariable x) = "variable " ++ varToString x ++ " is unbound"
errToStringᴱ (SEGV a x) = "address " ++ addrToString a ++ " is unallocated"
errToStringᴱ (app E) = errToStringᴱ E
errToStringᴱ (block b E) = errToStringᴮ E ++ "\n  in call of function " ++ varToString b

errToStringᴮ (local x E) = errToStringᴱ E ++ "\n  in definition of " ++ varToString (name x) 
errToStringᴮ (return E) = errToStringᴱ E ++ "\n  in return statement"
