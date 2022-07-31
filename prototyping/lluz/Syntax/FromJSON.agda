{-# OPTIONS --rewriting #-}

module Luau.Syntax.FromJSON where

open import Luau.Syntax using (Block; Stat ; Expr; _$_; val; nil; bool; number; var; var_∈_; function_is_end; _⟨_⟩; _⟨_⟩∈_; local_←_; return; done; _∙_; maybe; VarDec;  binexp; BinaryOperator; +; -; *; /; ==; ~=; <; >; <=; >=; ··; string)
open import Luau.Type.FromJSON using (typeFromJSON)

open import Agda.Builtin.List using (List; _∷_; [])
open import Agda.Builtin.Bool using (true; false)

open import FFI.Data.Aeson using (Value; Array; Object; object; array; string; fromString; lookup)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (Maybe; nothing; just)
open import FFI.Data.Scientific using (toFloat)
open import FFI.Data.String using (String; _++_)
open import FFI.Data.Vector using (head; tail; null; empty)

args = fromString "args"
body = fromString "body"
func = fromString "func"
lokal = fromString "local"
list = fromString "list"
name = fromString "name"
type = fromString "type"
value = fromString "value"
values = fromString "values"
vars = fromString "vars"
op = fromString "op"
left = fromString "left"
right = fromString "right"
returnAnnotation = fromString "returnAnnotation"
types = fromString "types"

data Lookup : Set where
  _,_ : String → Value → Lookup
  nil : Lookup

lookupIn : List String → Object → Lookup
lookupIn [] obj = nil
lookupIn (key ∷ keys) obj with lookup (fromString key) obj
lookupIn (key ∷ keys) obj | nothing = lookupIn keys obj
lookupIn (key ∷ keys) obj | just value = (key , value)

binOpFromJSON : Value → Either String BinaryOperator
binOpFromString : String → Either String BinaryOperator
varDecFromJSON : Value → Either String (VarDec maybe)
varDecFromObject : Object → Either String (VarDec maybe)
exprFromJSON : Value → Either String (Expr maybe)
exprFromObject : Object → Either String (Expr maybe)
statFromJSON : Value → Either String (Stat maybe)
statFromObject : Object → Either String (Stat maybe)
blockFromJSON : Value → Either String (Block maybe)
blockFromArray : Array → Either String (Block maybe)

binOpFromJSON (string s) = binOpFromString s
binOpFromJSON _ = Left "Binary operator not a string"

binOpFromString "Add" = Right +
binOpFromString "Sub" = Right -
binOpFromString "Mul" = Right *
binOpFromString "Div" = Right /
binOpFromString "CompareEq" = Right ==
binOpFromString "CompareNe" = Right ~=
binOpFromString "CompareLt" = Right <
binOpFromString "CompareLe" = Right <=
binOpFromString "CompareGt" = Right >
binOpFromString "CompareGe" = Right >=
binOpFromString "Concat" = Right ··
binOpFromString s = Left ("'" ++ s ++ "' is not a valid operator")

varDecFromJSON (object arg) = varDecFromObject arg
varDecFromJSON _ = Left "VarDec not an object"

varDecFromObject obj with lookup name obj | lookup type obj
varDecFromObject obj | just (string name) | nothing = Right (var name)
varDecFromObject obj | just (string name) | just Value.null = Right (var name)
varDecFromObject obj | just (string name) | just tyValue with typeFromJSON tyValue
varDecFromObject obj | just (string name) | just tyValue | Right ty = Right (var name ∈ ty)
varDecFromObject obj | just (string name) | just tyValue | Left err = Left err
varDecFromObject obj | just _ | _ = Left "AstLocal name is not a string"
varDecFromObject obj | nothing | _ = Left "AstLocal missing name"

exprFromJSON (object obj) = exprFromObject obj
exprFromJSON _ = Left "AstExpr not an object"

exprFromObject obj with lookup type obj
exprFromObject obj | just (string "AstExprCall") with lookup func obj | lookup args obj
exprFromObject obj | just (string "AstExprCall") | just value | just (array arr) with head arr
exprFromObject obj | just (string "AstExprCall") | just value | just (array arr) | just value2 with exprFromJSON value | exprFromJSON value2
exprFromObject obj | just (string "AstExprCall") | just value | just (array arr) | just value2 | Right fun | Right arg = Right (fun $ arg)
exprFromObject obj | just (string "AstExprCall") | just value | just (array arr) | just value2 | Left err | _ = Left err
exprFromObject obj | just (string "AstExprCall") | just value | just (array arr) | just value2 | _ | Left err = Left err
exprFromObject obj | just (string "AstExprCall") | just value | just (array arr) | nothing = Left ("AstExprCall empty args")
exprFromObject obj | just (string "AstExprCall") | just value | just _ = Left ("AstExprCall args not an array")
exprFromObject obj | just (string "AstExprCall") | nothing | _  = Left ("AstExprCall missing func")
exprFromObject obj | just (string "AstExprCall") | _ | nothing  = Left ("AstExprCall missing args")
exprFromObject obj | just (string "AstExprConstantNil") = Right (val nil)
exprFromObject obj | just (string "AstExprFunction") with lookup args obj | lookup body obj | lookup returnAnnotation obj
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | rtn with head arr | blockFromJSON blockValue
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | rtn | just argValue | Right B with varDecFromJSON argValue
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg with lookup types rtnObj
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg | just (array rtnTypes) with head rtnTypes
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg | just (array rtnTypes) | just rtnType with typeFromJSON rtnType
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg | just (array rtnTypes) | just rtnType | Left err = Left err
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg | just (array rtnTypes) | just rtnType | Right T = Right (function "" ⟨ arg ⟩∈ T is B end)
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg | just (array rtnTypes) | nothing = Right (function "" ⟨ arg ⟩ is B end)
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg | just _ = Left "returnAnnotation types not an array"
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just (object rtnObj) | just argValue | Right B | Right arg | nothing = Left "returnAnnotation missing types"
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | just _ | just argValue | Right B | Right arg  = Left "returnAnnotation not an object"
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | nothing | just argValue | Right B | Right arg = Right (function "" ⟨ arg ⟩ is B end)
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | rtn | just argValue | Right B | Left err = Left err
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | rtn | nothing | Right B = Left "Unsupported AstExprFunction empty args"
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just blockValue | rtn | _ | Left err = Left err
exprFromObject obj | just (string "AstExprFunction") | just _ | just _ | rtn = Left "AstExprFunction args not an array"
exprFromObject obj | just (string "AstExprFunction") | nothing | _ | rtn = Left "AstExprFunction missing args"
exprFromObject obj | just (string "AstExprFunction") | _ | nothing | rtn = Left "AstExprFunction missing body"
exprFromObject obj | just (string "AstExprLocal") with lookup lokal obj
exprFromObject obj | just (string "AstExprLocal") | just x with varDecFromJSON x
exprFromObject obj | just (string "AstExprLocal") | just x | Right x′ = Right (var (Luau.Syntax.name x′))
exprFromObject obj | just (string "AstExprLocal") | just x | Left err = Left err
exprFromObject obj | just (string "AstExprLocal") | nothing = Left "AstExprLocal missing local"
exprFromObject obj | just (string "AstExprConstantNumber") with lookup value obj
exprFromObject obj | just (string "AstExprConstantNumber") | just (FFI.Data.Aeson.Value.number x) = Right (val (number (toFloat x)))
exprFromObject obj | just (string "AstExprConstantNumber") | just _ = Left "AstExprConstantNumber value is not a number"
exprFromObject obj | just (string "AstExprConstantNumber") | nothing = Left "AstExprConstantNumber missing value"
exprFromObject obj | just (string "AstExprConstantString") with lookup value obj
exprFromObject obj | just (string "AstExprConstantString") | just (string x) = Right (val (string x))
exprFromObject obj | just (string "AstExprConstantString") | just _ = Left "AstExprConstantString value is not a string"
exprFromObject obj | just (string "AstExprConstantString") | nothing = Left "AstExprConstantString missing value"
exprFromObject obj | just (string "AstExprConstantBool") with lookup value obj
exprFromObject obj | just (string "AstExprConstantBool") | just (FFI.Data.Aeson.Value.bool b) = Right (val (bool b))
exprFromObject obj | just (string "AstExprConstantBool") | just _ = Left "AstExprConstantBool value is not a bool"
exprFromObject obj | just (string "AstExprConstantBool") | nothing = Left "AstExprConstantBool missing value"
exprFromObject obj | just (string "AstExprBinary") with lookup op obj | lookup left obj | lookup right obj
exprFromObject obj | just (string "AstExprBinary") | just o | just l | just r with binOpFromJSON o | exprFromJSON l | exprFromJSON r
exprFromObject obj | just (string "AstExprBinary") | just o | just l | just r | Right o′ | Right l′ | Right r′ = Right (binexp l′ o′ r′)
exprFromObject obj | just (string "AstExprBinary") | just o | just l | just r | Left err | _ | _ = Left err
exprFromObject obj | just (string "AstExprBinary") | just o | just l | just r | _ | Left err | _ = Left err
exprFromObject obj | just (string "AstExprBinary") | just o | just l | just r | _ | _ | Left err = Left err
exprFromObject obj | just (string "AstExprBinary") | nothing | _ | _ = Left "Missing 'op' in AstExprBinary"
exprFromObject obj | just (string "AstExprBinary") | _ | nothing | _ = Left "Missing 'left' in AstExprBinary"
exprFromObject obj | just (string "AstExprBinary") | _ | _ | nothing = Left "Missing 'right' in AstExprBinary"
exprFromObject obj | just (string ty) = Left ("TODO: Unsupported AstExpr " ++ ty)
exprFromObject obj | just _ = Left "AstExpr type not a string"
exprFromObject obj | nothing = Left "AstExpr missing type"

{-# NON_TERMINATING #-}
statFromJSON (object obj) = statFromObject obj
statFromJSON _ = Left "AstStat not an object"

statFromObject obj with lookup type obj
statFromObject obj | just(string "AstStatLocal") with lookup vars obj | lookup values obj
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) with head(arr1) | head(arr2)
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(x) | just(value) with varDecFromJSON(x) | exprFromJSON(value)
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(x) | just(value) | Right x′ | Right M = Right (local x′ ← M)
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(x) | just(value) | Left err | _ = Left err
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(x) | just(value) | _ | Left err = Left err
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(x) | nothing = Left "AstStatLocal empty values"
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | nothing | _ = Left "AstStatLocal empty vars"
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(_) = Left "AstStatLocal values not an array"
statFromObject obj | just(string "AstStatLocal") | just(_) | just(_) = Left "AstStatLocal vars not an array"
statFromObject obj | just(string "AstStatLocal") | just(_) | nothing = Left "AstStatLocal missing values"
statFromObject obj | just(string "AstStatLocal") | nothing | _ = Left "AstStatLocal missing vars"
statFromObject obj | just(string "AstStatLocalFunction") with lookup name obj | lookup func obj
statFromObject obj | just(string "AstStatLocalFunction") | just fnName | just value with varDecFromJSON fnName | exprFromJSON value
statFromObject obj | just(string "AstStatLocalFunction") | just fnName | just value | Right fnVar | Right (function "" ⟨ x ⟩ is B end) = Right (function (Luau.Syntax.name fnVar) ⟨ x ⟩ is B end)
statFromObject obj | just(string "AstStatLocalFunction") | just fnName | just value | Right fnVar | Right (function "" ⟨ x ⟩∈ T is B end) = Right (function (Luau.Syntax.name fnVar) ⟨ x ⟩∈ T is B end)
statFromObject obj | just(string "AstStatLocalFunction") | just fnName | just value | Left err | _ = Left err
statFromObject obj | just(string "AstStatLocalFunction") | just fnName | just value | _ | Left err = Left err
statFromObject obj | just(string "AstStatLocalFunction") | just _ | just _ | Right _ | Right _ = Left "AstStatLocalFunction func is not an AstExprFunction"
statFromObject obj | just(string "AstStatLocalFunction") | nothing | _ = Left "AstStatFunction missing name"
statFromObject obj | just(string "AstStatLocalFunction") | _ | nothing = Left "AstStatFunction missing func"
statFromObject obj | just(string "AstStatReturn") with lookup list obj
statFromObject obj | just(string "AstStatReturn") | just(array arr) with head arr
statFromObject obj | just(string "AstStatReturn") | just(array arr) | just value with exprFromJSON value
statFromObject obj | just(string "AstStatReturn") | just(array arr) | just value | Right M = Right (return M)
statFromObject obj | just(string "AstStatReturn") | just(array arr) | just value | Left err = Left err
statFromObject obj | just(string "AstStatReturn") | just(array arr) | nothing = Left "AstStatReturn empty list"
statFromObject obj | just(string "AstStatReturn") | just(_) = Left "AstStatReturn list not an array"
statFromObject obj | just(string "AstStatReturn") | nothing = Left "AstStatReturn missing list"
statFromObject obj | just (string ty) = Left ("TODO: Unsupported AstStat " ++ ty)
statFromObject obj | just _ = Left "AstStat type not a string"
statFromObject obj | nothing = Left "AstStat missing type"

blockFromJSON (array arr) = blockFromArray arr
blockFromJSON (object obj) with lookup type obj | lookup body obj
blockFromJSON (object obj) | just (string "AstStatBlock") | just value = blockFromJSON value
blockFromJSON (object obj) | just (string "AstStatBlock") | nothing = Left "AstStatBlock missing body"
blockFromJSON (object obj) | just (string ty) | _ = Left ("Unsupported AstBlock " ++ ty)
blockFromJSON (object obj) | just _ | _ = Left "AstStatBlock type not a string"
blockFromJSON (object obj) | nothing | _ = Left "AstStatBlock missing type"
blockFromJSON _ = Left "AstBlock not an array or AstStatBlock object"

blockFromArray arr with head arr
blockFromArray arr | nothing = Right done
blockFromArray arr | just value with statFromJSON value
blockFromArray arr | just value | Left err = Left err
blockFromArray arr | just value | Right S with blockFromArray(tail arr)
blockFromArray arr | just value | Right S | Left err = Left (err)
blockFromArray arr | just value | Right S | Right B  = Right (S ∙ B)
   
