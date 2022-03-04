{-# OPTIONS --rewriting #-}

module Luau.RuntimeError.ToString where

open import Agda.Builtin.Float using (primShowFloat)
open import FFI.Data.String using (String; _++_)
open import Luau.RuntimeError using (RuntimeErrorᴮ; RuntimeErrorᴱ; local; return; FunctionMismatch; BinOpMismatch₁; BinOpMismatch₂; UnboundVariable; SEGV; app₁; app₂; block; bin₁; bin₂)
open import Luau.RuntimeType.ToString using (runtimeTypeToString)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Syntax.ToString using (valueToString; exprToString)
open import Luau.Var.ToString using (varToString)
open import Luau.Syntax using (var; val; addr; binexp; block_is_end; local_←_; return; _∙_; name; _$_; ··)

errToStringᴱ : ∀ {a H} M → RuntimeErrorᴱ {a} H M → String
errToStringᴮ : ∀ {a H} B → RuntimeErrorᴮ {a} H B → String

errToStringᴱ (var x) (UnboundVariable) = "variable " ++ varToString x ++ " is unbound"
errToStringᴱ (val (addr a)) (SEGV p) = "address " ++ addrToString a ++ " is unallocated"
errToStringᴱ (M $ N) (FunctionMismatch v w p) = "value " ++ (valueToString v) ++ " is not a function"
errToStringᴱ (M $ N) (app₁ E) = errToStringᴱ M E
errToStringᴱ (M $ N) (app₂ E) = errToStringᴱ N E
errToStringᴱ (binexp M ·· N) (BinOpMismatch₁ v w p) = "value " ++ (valueToString v) ++ " is not a string"
errToStringᴱ (binexp M ·· N) (BinOpMismatch₂ v w p) = "value " ++ (valueToString w) ++ " is not a string"
errToStringᴱ (binexp M op N) (BinOpMismatch₁ v w p) = "value " ++ (valueToString v) ++ " is not a number"
errToStringᴱ (binexp M op N) (BinOpMismatch₂ v w p) = "value " ++ (valueToString w) ++ " is not a number"
errToStringᴱ (binexp M op N) (bin₁ E) = errToStringᴱ M E
errToStringᴱ (binexp M op N) (bin₂ E) = errToStringᴱ N E
errToStringᴱ (block b is B end) (block E) = errToStringᴮ B E ++ "\n  in call of function " ++ varToString (name b)

errToStringᴮ (local x ← M ∙ B) (local E) = errToStringᴱ M E ++ "\n  in definition of " ++ varToString (name x) 
errToStringᴮ (return M ∙ B) (return E) = errToStringᴱ M E ++ "\n  in return statement"
