module Examples.Syntax where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.String using (_++_)
open import Luau.Syntax using (var; _$_; return; nil; function_⟨_⟩_end_)
open import Luau.Syntax.ToString using (exprToString; blockToString)

f = var "f"
x = var "x"

ex1 : exprToString(f $ x) ≡
  "f(x)"
ex1 = refl

ex2 : blockToString(return nil) ≡
  "return nil"
ex2 = refl

ex3 : blockToString(function "f" ⟨ "x" ⟩ return x end return f) ≡
  "function f(x)\n" ++
  "  return x\n" ++
  "end\n" ++
  "return f"
ex3 = refl
