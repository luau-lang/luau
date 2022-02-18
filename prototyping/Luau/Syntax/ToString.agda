module Luau.Syntax.ToString where

open import Luau.Syntax using (Block; Stat; Expr; VarDec; FunDec; nil; var; var_∈_; addr; _$_; function_is_end; return; local_←_; _∙_; done; block_is_end; _⟨_⟩; _⟨_⟩∈_)
open import FFI.Data.String using (String; _++_)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Type.ToString using (typeToString)
open import Luau.Var.ToString using (varToString)

varDecToString : ∀ {a} → VarDec a → String
varDecToString (var x) = varToString x
varDecToString (var x ∈ T) =  varToString x ++ " : " ++ typeToString T

funDecToString : ∀ {a} → FunDec a → String
funDecToString (f ⟨ x ⟩∈ T) = varToString f ++ "(" ++ varDecToString x ++ "): " ++ typeToString T
funDecToString (f ⟨ x ⟩) = varToString f ++ "(" ++ varDecToString x ++ ")"

exprToString′ : ∀ {a} → String → Expr a → String
statToString′ : ∀ {a} → String → Stat a → String
blockToString′ : ∀ {a} → String → Block a → String

exprToString′ lb nil =
  "nil"
exprToString′ lb (addr a) =
  addrToString(a)
exprToString′ lb (var x) =
  varToString(x)
exprToString′ lb (M $ N) =
  (exprToString′ lb M) ++ "(" ++ (exprToString′ lb N) ++ ")"
exprToString′ lb (function F is B end) =
  "function " ++ funDecToString F ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"
exprToString′ lb (block (var b) is B end) =
  "(function " ++ b ++ "()" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end)()"
exprToString′ lb (block (var b ∈ T) is B end) =
  "(function " ++ b ++ "(): " ++ typeToString(T) ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end)()"

statToString′ lb (function F is B end) =
  "local function " ++ funDecToString F ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"
statToString′ lb (local x ← M) =
  "local " ++ varDecToString x ++ " = " ++ (exprToString′ lb M)
statToString′ lb (return M) =
  "return " ++ (exprToString′ lb M)

blockToString′ lb (S ∙ done) = statToString′ lb S
blockToString′ lb (S ∙ B) = statToString′ lb S ++ lb ++ blockToString′ lb B
blockToString′ lb (done) = ""

exprToString : ∀ {a} → Expr a → String
exprToString = exprToString′ "\n"

statToString : ∀ {a} → Stat a → String
statToString = statToString′ "\n"

blockToString : ∀ {a} → Block a → String
blockToString = blockToString′ "\n"
