module Luau.Syntax.FromJSON where

open import Luau.Syntax using (Type; Block; Stat ; Expr; nil; return; _∙; _∙_)

open import Agda.Builtin.List using (List; _∷_; [])

open import FFI.Data.Aeson using (Value; Array; Object; object; array; fromString; lookup)
open import FFI.Data.Bool using (true; false)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (Maybe; nothing; just)
open import FFI.Data.String using (String; _++_)
open import FFI.Data.Vector using (head; tail; null; empty)

AstExprKeys =
  "AstExprBinary" ∷
  "AstExprCall" ∷
  "AstExprConstantBool" ∷
  "AstExprConstantNil" ∷
  "AstExprConstantNumber" ∷
  "AstExprConstantString" ∷
  "AstExprError" ∷
  "AstExprFunction" ∷
  "AstExprGlobal" ∷
  "AstExprGroup" ∷
  "AstExprIndexExpr" ∷
  "AstExprIndexName" ∷
  "AstExprLocal" ∷
  "AstExprTable" ∷
  "AstExprTypeAssertion" ∷
  "AstExprUnary" ∷
  []

AstStatKeys =
  "AstStatAssign" ∷
  "AstStatBlock" ∷
  "AstStatBreak" ∷
  "AstStatCompoundAssign" ∷
  "AstStatContinue" ∷
  "AstStatDeclareClass" ∷
  "AstStatDeclareFunction" ∷
  "AstStatDeclareGlobal" ∷
  "AstStatError" ∷
  "AstStatExpr" ∷
  "AstStatFor" ∷
  "AstStatForIn" ∷
  "AstStatFunction" ∷
  "AstStatIf" ∷
  "AstStatLocal" ∷
  "AstStatLocalFunction" ∷
  "AstStatRepeat" ∷
  "AstStatReturn" ∷
  "AstStatTypeAlias" ∷
  []

AstTypeKeys =
  "AstTypeError" ∷
  "AstTypeFunction" ∷
  "AstTypeIntersection" ∷
  "AstTypePackExplicit" ∷
  "AstTypePackGeneric" ∷
  "AstTypePackVariadic" ∷
  "AstTypeReference" ∷
  "AstTypeTable" ∷
  "AstTypeTypeof" ∷
  "AstTypeUnion" ∷
  []

data Lookup : Set where
  _,_ : String → Value → Lookup
  nil : Lookup

lookupIn : List String → Object → Lookup
lookupIn [] obj = nil
lookupIn (key ∷ keys) obj with lookup (fromString key) obj
lookupIn (key ∷ keys) obj | nothing = lookupIn keys obj
lookupIn (key ∷ keys) obj | just value = (key , value)

exprFromJSON : Value → Either String Expr
exprFromObject : Object → Either String Expr
statFromJSON : Value → Either String Stat
statFromObject : Object → Either String Stat
blockFromJSON : Value → Either String Block
blockFromArray : Array → Either String Block

exprFromJSON (object obj) = exprFromObject obj
exprFromJSON val = Left "Expr should be an object"

exprFromObject obj with lookupIn AstExprKeys obj
exprFromObject obj | ("AstExprConstantNil" , value) = Right nil
exprFromObject obj | (key , value) = Left ("Unsupported " ++ key)
exprFromObject obj | nil = Left "Unexpected Expr object"

{-# NON_TERMINATING #-}
statFromJSON (object obj) = statFromObject obj
statFromJSON _ = Left "Stat should be an object"

statFromObject obj with lookupIn AstStatKeys obj
statFromObject obj | ("AstStatReturn" , value) with exprFromJSON value
statFromObject obj | ("AstStatReturn" , value) | Left err = Left err
statFromObject obj | ("AstStatReturn" , value) | Right expr = Right (return expr)
statFromObject obj | (key , value) = Left ("Unsupported " ++ key)
statFromObject obj | nil = Left "Unexpected Stat object"

blockFromJSON (array arr) = blockFromArray arr
blockFromJSON _ = Left "Block should be an array"

blockFromArray arr with head arr
blockFromArray arr | nothing = Left "Block should be a non-empty array"
blockFromArray arr | just value with statFromJSON value
blockFromArray arr | just value | Left err = Left err
blockFromArray arr | just value | Right S with null (tail arr)
blockFromArray arr | just value | Right S | true = Right (S ∙)
blockFromArray arr | just value | Right S | false with blockFromArray(tail arr)
blockFromArray arr | just value | Right S | false | Left err = Left (err)
blockFromArray arr | just value | Right S | false | Right B  = Right (S ∙ B)
