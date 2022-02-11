module Luau.Syntax.ToString where

open import Luau.Syntax using (Block; Stat; Expr; nil; var; addr; _$_; function⟨_⟩_end; return; function_⟨_⟩_end ;local_←_; _∙_; done; block_is_end)
open import FFI.Data.String using (String; _++_)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Var.ToString using (varToString)

exprToString′ : String → Expr → String
statToString′ : String → Stat → String
blockToString′ : String → Block → String

exprToString′ lb nil =
  "nil"
exprToString′ lb (addr a) =
  addrToString(a)
exprToString′ lb (var x) =
  varToString(x)
exprToString′ lb (M $ N) =
  (exprToString′ lb M) ++ "(" ++ (exprToString′ lb N) ++ ")"
exprToString′ lb (function⟨ x ⟩ B end) =
  "function(" ++ x ++ ")" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"
exprToString′ lb (block b is B end) =
  "(function " ++ b ++ "()" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end)()"

statToString′ lb (function f ⟨ x ⟩ B end) =
  "local function " ++ f ++ "(" ++ x ++ ")" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"
statToString′ lb (local x ← M) =
  "local " ++ x ++ " = " ++ (exprToString′ lb M)
statToString′ lb (return M) =
  "return " ++ (exprToString′ lb M)

blockToString′ lb (S ∙ done) = statToString′ lb S
blockToString′ lb (S ∙ B) = statToString′ lb S ++ lb ++ blockToString′ lb B
blockToString′ lb (done) = ""

exprToString : Expr → String
exprToString = exprToString′ "\n"

statToString : Stat → String
statToString = statToString′ "\n"

blockToString : Block → String
blockToString = blockToString′ "\n"
