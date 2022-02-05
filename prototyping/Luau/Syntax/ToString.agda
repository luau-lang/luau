module Luau.Syntax.ToString where

open import Luau.Syntax using (Type; Block; Stat; Expr; nil; var; _$_; return ; function_⟨_⟩_end ; local_←_; _∙_; _∙)

open import FFI.Data.String using (String; _++_)

exprToString : Expr → String
exprToString nil = "nil"
exprToString (var x) = x
exprToString (M $ N) = (exprToString M) ++ "(" ++ (exprToString N) ++ ")"

statToString′ : String → Stat → String
blockToString′ : String → Block → String

statToString′ lb (function f ⟨ x ⟩ B end) =
  "function " ++ f ++ "(" ++ x ++ ")" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"
statToString′ lb (local x ← M) =
  "local " ++ x ++ " = " ++ (exprToString M)
statToString′ lb (return M) =
  "return " ++ (exprToString M)

blockToString′ lb (S ∙ B) = statToString′ lb S ++ lb ++ blockToString′ lb B
blockToString′ lb (S ∙) = statToString′ lb S

statToString : Stat → String
statToString = statToString′ "\n"

blockToString : Block → String
blockToString = blockToString′ "\n"
