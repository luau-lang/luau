{-# OPTIONS --rewriting #-}

module Luau.StrictMode.ToString where

open import Agda.Builtin.Nat using (Nat; suc)
open import FFI.Data.String using (String; _++_)
open import Luau.Subtyping using (_≮:_; Tree; witness; scalar; function; function-ok; function-err; function-tgt)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; UnallocatedAddress; UnboundVariable; FunctionCallMismatch; FunctionDefnMismatch; BlockMismatch; app₁; app₂; BinOpMismatch₁; BinOpMismatch₂; bin₁; bin₂; block₁; return; LocalVarMismatch; local₁; local₂; function₁; function₂; heap; expr; block; addr)
open import Luau.Syntax using (Expr; val; yes; var; var_∈_; _⟨_⟩∈_; _$_; addr; number; binexp; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg; name)
open import Luau.Type using (number; boolean; string; nil)
open import Luau.TypeCheck using (_⊢ᴮ_∈_; _⊢ᴱ_∈_)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Var.ToString using (varToString)
open import Luau.Type.ToString using (typeToString)
open import Luau.Syntax.ToString using (binOpToString)

tmp : Nat → String
tmp 0 = "w"
tmp 1 = "x"
tmp 2 = "y"
tmp 3 = "z"
tmp (suc (suc (suc n))) = tmp n ++ "'"

treeToString : Tree → Nat → String → String
treeToString (scalar number) n v = v ++ " is a number"
treeToString (scalar boolean) n v = v ++ " is a boolean"
treeToString (scalar string) n v = v ++ " is a string"
treeToString (scalar nil) n v = v ++ " is nil"
treeToString function n v = v ++ " is a function"
treeToString (function-ok s t) n v = treeToString t (suc n) (v ++ "(" ++ w ++ ")") ++ " when\n  " ++ treeToString s (suc n) w where w = tmp n
treeToString (function-err t) n v = v ++ "(" ++ w ++ ") can error when\n  " ++ treeToString t (suc n) w where w = tmp n
treeToString (function-tgt t) n v = treeToString t n (v ++ "()")

subtypeWarningToString : ∀ {T U} → (T ≮: U) → String
subtypeWarningToString (witness t p q) = "\n  because provided type contains v, where " ++ treeToString t 0 "v"

warningToStringᴱ : ∀ {H Γ T} M → {D : Γ ⊢ᴱ M ∈ T} → Warningᴱ H D → String
warningToStringᴮ : ∀ {H Γ T} B → {D : Γ ⊢ᴮ B ∈ T} → Warningᴮ H D → String

warningToStringᴱ (var x) (UnboundVariable p) = "Unbound variable " ++ varToString x
warningToStringᴱ (val (addr a)) (UnallocatedAddress p) = "Unallocated address " ++ addrToString a
warningToStringᴱ (M $ N) (FunctionCallMismatch {T = T} {U = U} p) = "Function has type " ++ typeToString T ++ " but argument has type " ++ typeToString U ++ subtypeWarningToString p
warningToStringᴱ (M $ N) (app₁ W) = warningToStringᴱ M W
warningToStringᴱ (M $ N) (app₂ W) = warningToStringᴱ N W
warningToStringᴱ (function f ⟨ var x ∈ T ⟩∈ U is B end) (FunctionDefnMismatch {V = V} p) = "Function expresion " ++ varToString f ++ " has return type " ++ typeToString U ++ " but body returns " ++ typeToString V ++ subtypeWarningToString p
warningToStringᴱ (function f ⟨ var x ∈ T ⟩∈ U is B end) (function₁ W) = warningToStringᴮ B W ++ "\n  in function expression " ++ varToString f
warningToStringᴱ block var b ∈ T is B end (BlockMismatch {U = U} p) =  "Block " ++ varToString b ++ " has type " ++ typeToString T ++ " but body returns " ++ typeToString U ++ subtypeWarningToString p
warningToStringᴱ block var b ∈ T is B end (block₁ W) = warningToStringᴮ B W ++ "\n  in block " ++ varToString b
warningToStringᴱ (binexp M op N) (BinOpMismatch₁ {T = T} p) = "Binary operator " ++ binOpToString op ++ " lhs has type " ++ typeToString T ++ subtypeWarningToString p
warningToStringᴱ (binexp M op N) (BinOpMismatch₂ {U = U} p) = "Binary operator " ++ binOpToString op ++ " rhs has type " ++ typeToString U ++ subtypeWarningToString p
warningToStringᴱ (binexp M op N) (bin₁ W) = warningToStringᴱ M W
warningToStringᴱ (binexp M op N) (bin₂ W) = warningToStringᴱ N W

warningToStringᴮ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (FunctionDefnMismatch {V = V} p) = "Function declaration " ++ varToString f ++ " has return type " ++ typeToString U ++ " but body returns " ++ typeToString V ++ subtypeWarningToString p
warningToStringᴮ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function₁ W) = warningToStringᴮ C W ++ "\n  in function declaration " ++ varToString f
warningToStringᴮ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function₂ W) = warningToStringᴮ B W
warningToStringᴮ (local var x ∈ T ← M ∙ B) (LocalVarMismatch {U = U} p) =  "Local variable " ++ varToString x ++ " has type " ++ typeToString T ++ " but expression has type " ++ typeToString U ++ subtypeWarningToString p
warningToStringᴮ (local var x ∈ T ← M ∙ B) (local₁ W) = warningToStringᴱ M W ++ "\n  in local variable declaration " ++ varToString x
warningToStringᴮ (local var x ∈ T ← M ∙ B) (local₂ W) = warningToStringᴮ B W
warningToStringᴮ (return M ∙ B) (return W) = warningToStringᴱ M W ++ "\n  in return statement"

