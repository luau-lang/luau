module Luau.Syntax.FromJSON where

open import Luau.Syntax using (Type; Block; Stat ; Expr; nil; _$_; var; function⟨_⟩_end; local_←_; function_⟨_⟩_end; return; _∙; _∙_)

open import Agda.Builtin.List using (List; _∷_; [])

open import FFI.Data.Aeson using (Value; Array; Object; object; array; string; fromString; lookup)
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

args = fromString "args"
body = fromString "body"
func = fromString "func"
name = fromString "name"
values = fromString "values"
vars = fromString "vars"

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
exprFromObject obj | ("AstExprCall" , object obj2) with lookup func obj2 | lookup args obj2
exprFromObject obj | ("AstExprCall" , object obj2) | just value | just (array arr) with head arr
exprFromObject obj | ("AstExprCall" , object obj2) | just value | just (array arr) | just value2 with exprFromJSON value | exprFromJSON value2
exprFromObject obj | ("AstExprCall" , object obj2) | just value | just (array arr) | just value2 | Right fun | Right arg = Right (fun $ arg)
exprFromObject obj | ("AstExprCall" , object obj2) | just value | just (array arr) | just value2 | Left err | _ = Left err
exprFromObject obj | ("AstExprCall" , object obj2) | just value | just (array arr) | just value2 | _ | Left err = Left err
exprFromObject obj | ("AstExprCall" , object obj2) | just value | just (array arr) | nothing = Left ("AstExprCall empty args")
exprFromObject obj | ("AstExprCall" , object obj2) | just value | just _ = Left ("AstExprCall args not an array")
exprFromObject obj | ("AstExprCall" , object obj2) | nothing | _  = Left ("AstExprCall missing func")
exprFromObject obj | ("AstExprCall" , object obj2) | _ | nothing  = Left ("AstExprCall missing args")
exprFromObject obj | ("AstExprConstantNil" , value) = Right nil
exprFromObject obj | ("AstExprFunction" , object obj2 ) with lookup args obj2 | lookup func obj2
exprFromObject obj | ("AstExprFunction" , object obj2 ) | just (array arr) | just value with head arr | blockFromJSON value
exprFromObject obj | ("AstExprFunction" , object obj2 ) | just (array arr) | just value | just (string x) | Right B = Right (function⟨ x ⟩ B end)
exprFromObject obj | ("AstExprFunction" , object obj2 ) | just (array arr) | just value | just _ | Right B = Left "AstExprFunction args not a string array"
exprFromObject obj | ("AstExprFunction" , object obj2 ) | just (array arr) | just value | nothing | Right B = Left "Unsupported AstExprFunction empty args"
exprFromObject obj | ("AstExprFunction" , object obj2 ) | just (array arr) | just value | _ | Left err = Left err
exprFromObject obj | ("AstExprFunction" , object obj2 ) | just _ | just _ = Left "AstExprFunction args not an array"
exprFromObject obj | ("AstExprFunction" , object obj2 ) | nothing | _ = Left "AstExprFunction missing args"
exprFromObject obj | ("AstExprFunction" , object obj2 ) | _ | nothing = Left "AstExprFunction missing body"
exprFromObject obj | ("AstExprLocal" , string x) = Right (var x)
exprFromObject obj | (key , value) = Left ("Unsupported " ++ key)
exprFromObject obj | nil = Left "Unexpected Expr object"

{-# NON_TERMINATING #-}
statFromJSON (object obj) = statFromObject obj
statFromJSON _ = Left "Stat should be an object"

statFromObject obj with lookupIn AstStatKeys obj
statFromObject obj | ("AstStatLocal" , object obj2) with lookup vars obj2 | lookup values obj2
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(array arr2) with head(arr1) | head(arr2)
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(array arr2) | just(string x) | just(value) with exprFromJSON(value)
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(array arr2) | just(string x) | just(value) | Right M = Right (local x ← M)
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(array arr2) | just(string x) | just(value) | Left err = Left err
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(array arr2) | just(string x) | nothing = Left "AstStatLocal empty values"
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(array arr2) | just(_) | _ = Left "AstStatLocal vars not a string array"
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(array arr2) | nothing | _ = Left "AstStatLocal emptyvars"
statFromObject obj | ("AstStatLocal" , object obj2) | just(array arr1) | just(_) = Left "AstStatLocal values not an array"
statFromObject obj | ("AstStatLocal" , object obj2) | just(_) | just(_) = Left "AstStatLocal vars not an array"
statFromObject obj | ("AstStatLocal" , object obj2) | just(_) | nothing = Left "AstStatLocal missing values"
statFromObject obj | ("AstStatLocal" , object obj2) | nothing | _ = Left "AstStatLocal missing vars"
statFromObject obj | ("AstStatLocalFunction" , object obj2) with lookup name obj2 | lookup func obj2
statFromObject obj | ("AstStatLocalFunction" , object obj2) | just (string f) | just value with exprFromJSON value
statFromObject obj | ("AstStatLocalFunction" , object obj2) | just (string f) | just value | Right (function⟨ x ⟩ B end) = Right (function f ⟨ x ⟩ B end)
statFromObject obj | ("AstStatLocalFunction" , object obj2) | just (string f) | just value | Left err = Left err
statFromObject obj | ("AstStatLocalFunction" , object obj2) | just _ | just _ | Right _ = Left "AstStatLocalFunction func is not an AstExprFunction"
statFromObject obj | ("AstStatLocalFunction" , object obj2) | just _ | just _ =  Left "AstStatLocalFunction name is not a string"
statFromObject obj | ("AstStatLocalFunction" , object obj2) | nothing | _ = Left "AstStatFunction missing name"
statFromObject obj | ("AstStatLocalFunction" , object obj2) | _ | nothing = Left "AstStatFunction missing func"
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
