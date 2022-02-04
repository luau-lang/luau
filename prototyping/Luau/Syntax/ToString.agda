module Luau.Syntax.ToString where

open import Luau.Syntax using (Type; Block; Expr; nil; var; _$_; return ; function_⟨_⟩_end_ ; local_←_∙_)

open import FFI.Data.String using (String; _++_)

exprToString : Expr → String
exprToString nil = "nil"
exprToString (var x) = x
exprToString (M $ N) = (exprToString M) ++ "(" ++ (exprToString N) ++ ")"

blockToString′ : String → Block → String
blockToString′ lb (function f ⟨ x ⟩ B end C) =
  "function " ++ f ++ "(" ++ x ++ ")" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end" ++ lb ++
  blockToString′ lb C
blockToString′ lb (local x ← M ∙ B) =
  "local " ++ x ++ " = " ++ (exprToString M) ++ lb ++
  (blockToString′ lb B)
blockToString′ lb (return M) =
  "return " ++ (exprToString M)

blockToString : Block → String
blockToString = blockToString′ "\n"




