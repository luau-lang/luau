module Luau.Syntax.ToString where

open import Agda.Builtin.Bool using (true; false)
open import Agda.Builtin.Float using (primShowFloat)
open import Agda.Builtin.String using (primShowString)
open import Luau.Syntax using (Value; Block; Stat; Expr; VarDec; FunDec; nil; bool; val; var; var_∈_; addr; _$_; function_is_end; return; local_←_; _∙_; done; block_is_end; _⟨_⟩; _⟨_⟩∈_; number; BinaryOperator; +; -; *; /; <; >; ==; ~=; <=; >=; ··; binexp; string)
open import FFI.Data.String using (String; _++_)
open import Luau.Addr.ToString using (addrToString)
open import Luau.Type.ToString using (typeToString)
open import Luau.Var.ToString using (varToString)

varDecToString : ∀ {a} → VarDec a → String
varDecToString (var x) = varToString x
varDecToString (var x ∈ T) =  varToString x ++ " : " ++ typeToString T

funDecToString : ∀ {a} → FunDec a → String
funDecToString ("" ⟨ x ⟩∈ T) = "function(" ++ varDecToString x ++ "): " ++ typeToString T
funDecToString ("" ⟨ x ⟩) = "function(" ++ varDecToString x ++ ")"
funDecToString (f ⟨ x ⟩∈ T) = "function " ++ varToString f ++ "(" ++ varDecToString x ++ "): " ++ typeToString T
funDecToString (f ⟨ x ⟩) = "function " ++ varToString f ++ "(" ++ varDecToString x ++ ")"

binOpToString : BinaryOperator → String
binOpToString + = "+"
binOpToString - = "-"
binOpToString * = "*"
binOpToString / = "/"
binOpToString < = "<"
binOpToString > = ">"
binOpToString == = "=="
binOpToString ~= = "~="
binOpToString <= = "<="
binOpToString >= = ">="
binOpToString ·· = ".."

valueToString : Value → String
valueToString nil = "nil"
valueToString (addr a) = addrToString a
valueToString (number x) = primShowFloat x
valueToString (bool false) = "false"
valueToString (bool true) = "true"
valueToString (string x) = primShowString x

exprToString′ : ∀ {a} → String → Expr a → String
statToString′ : ∀ {a} → String → Stat a → String
blockToString′ : ∀ {a} → String → Block a → String

exprToString′ lb (val v) =
  valueToString(v)
exprToString′ lb (var x) =
  varToString(x)
exprToString′ lb (M $ N) =
  (exprToString′ lb M) ++ "(" ++ (exprToString′ lb N) ++ ")"
exprToString′ lb (function F is B end) =
  funDecToString F ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end"
exprToString′ lb (block b is B end) =
  "(" ++ varDecToString b ++ "()" ++ lb ++
  "  " ++ (blockToString′ (lb ++ "  ") B) ++ lb ++
  "end)()"
exprToString′ lb (binexp x op y) = exprToString′ lb x ++ " " ++ binOpToString op ++ " " ++ exprToString′ lb y

statToString′ lb (function F is B end) =
  "local " ++ funDecToString F ++ lb ++
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
