module Luau.Syntax.ToString where

open import Luau.Syntax using (Type; Block; Stat; Expr; nil; var; _$_; function⟨_⟩_end; return; function_⟨_⟩_end ;local_←_; _∙_; _∙)

open import FFI.Data.String using (String; _++_)

exprToString′ : String → Expr → String
statToString′ : String → Stat → String
blockToString′ : String → Block → String

exprToString′ lb nil =
  "nil"
exprToString′ lb (var x) =
  x
exprToString′ lb (M $ N) =
  (exprToString′ lb M) ++ "(" ++ (exprToString′ lb N) ++ ")"
exprToString′ lb (function⟨ x ⟩ B end) =
  "function(" ++ x ++ ")" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"

statToString′ lb (function f ⟨ x ⟩ B end) =
  "local function " ++ f ++ "(" ++ x ++ ")" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"
statToString′ lb (local x ← M) =
  "local " ++ x ++ " = " ++ (exprToString′ lb M)
statToString′ lb (return M) =
  "return " ++ (exprToString′ lb M)

blockToString′ lb (S ∙ B) = statToString′ lb S ++ lb ++ blockToString′ lb B
blockToString′ lb (S ∙) = statToString′ lb S

exprToString : Expr → String
exprToString = exprToString′ "\n"

statToString : Stat → String
statToString = statToString′ "\n"

blockToString : Block → String
blockToString = blockToString′ "\n"
