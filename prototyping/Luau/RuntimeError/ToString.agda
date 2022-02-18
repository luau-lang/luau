{-# OPTIONS --rewriting #-}

module Luau.RuntimeError.ToString where

open import FFI.Data.String using (String; _++_)
open import Luau.RuntimeError using (RuntimeErrorᴮ; RuntimeErrorᴱ; local; return; NilIsNotAFunction; UnboundVariable; SEGV; app₁; app₂; block)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Var.ToString using (varToString)
open import Luau.Syntax using (name)

errToStringᴱ : ∀ {a H B} → RuntimeErrorᴱ {a} H B → String
errToStringᴮ : ∀ {a H B} → RuntimeErrorᴮ {a} H B → String

errToStringᴱ (NilIsNotAFunction) = "nil is not a function"
errToStringᴱ (UnboundVariable x) = "variable " ++ varToString x ++ " is unbound"
errToStringᴱ (SEGV a x) = "address " ++ addrToString a ++ " is unallocated"
errToStringᴱ (app₁ E) = errToStringᴱ E
errToStringᴱ (app₂ E) = errToStringᴱ E
errToStringᴱ (block b E) = errToStringᴮ E ++ "\n  in call of function " ++ varToString (name b)

errToStringᴮ (local x E) = errToStringᴱ E ++ "\n  in definition of " ++ varToString (name x) 
errToStringᴮ (return E) = errToStringᴱ E ++ "\n  in return statement"
