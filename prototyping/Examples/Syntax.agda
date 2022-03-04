module Examples.Syntax where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.String using (_++_)
open import Luau.Syntax using (var; _$_; return; val; nil; function_is_end; local_←_; done; _∙_; _⟨_⟩)
open import Luau.Syntax.ToString using (exprToString; blockToString)

ex1 : exprToString(function "" ⟨ var "x" ⟩ is return (var "f" $ var "x") ∙ done end) ≡
  "function(x)\n" ++
  "  return f(x)\n" ++
  "end"
ex1 = refl

ex2 : blockToString(local var "x" ← (val nil) ∙ return (var "x") ∙ done) ≡
  "local x = nil\n" ++
  "return x"
ex2 = refl

ex3 : blockToString(function "f" ⟨ var "x" ⟩ is return (var "x") ∙ done end ∙ return (var "f") ∙ done) ≡
  "local function f(x)\n" ++
  "  return x\n" ++
  "end\n" ++
  "return f"
ex3 = refl
