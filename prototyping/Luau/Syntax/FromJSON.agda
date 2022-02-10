module Luau.Syntax.FromJSON where

open import Luau.Syntax using (Block; Stat ; Expr; nil; _$_; var; function⟨_⟩_end; local_←_; function_⟨_⟩_end; return; done; _∙_)

open import Agda.Builtin.List using (List; _∷_; [])

open import FFI.Data.Aeson using (Value; Array; Object; object; array; string; fromString; lookup)
open import FFI.Data.Bool using (true; false)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (Maybe; nothing; just)
open import FFI.Data.String using (String; _++_)
open import FFI.Data.Vector using (head; tail; null; empty)

args = fromString "args"
body = fromString "body"
func = fromString "func"
lokal = fromString "local"
list = fromString "list"
name = fromString "name"
type = fromString "type"
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
exprFromJSON val = Left "AstExpr not an object"

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
exprFromObject obj | just (string "AstExprConstantNil") = Right nil
exprFromObject obj | just (string "AstExprFunction") with lookup args obj | lookup body obj
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just value with head arr | blockFromJSON value
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just value | just (string x) | Right B = Right (function⟨ var x ⟩ B end)
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just value | just _ | Right B = Left "AstExprFunction args not a string array"
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just value | nothing | Right B = Left "Unsupported AstExprFunction empty args"
exprFromObject obj | just (string "AstExprFunction") | just (array arr) | just value | _ | Left err = Left err
exprFromObject obj | just (string "AstExprFunction") | just _ | just _ = Left "AstExprFunction args not an array"
exprFromObject obj | just (string "AstExprFunction") | nothing | _ = Left "AstExprFunction missing args"
exprFromObject obj | just (string "AstExprFunction") | _ | nothing = Left "AstExprFunction missing body"
exprFromObject obj | just (string "AstExprLocal") with lookup lokal obj
exprFromObject obj | just (string "AstExprLocal") | just (string x) = Right (var x)
exprFromObject obj | just (string "AstExprLocal") | just (_) = Left "AstExprLocal local not a string"
exprFromObject obj | just (string "AstExprLocal") | nothing = Left "AstExprLocal missing local"
exprFromObject obj | just (string ty) = Left ("TODO: Unsupported AstExpr " ++ ty)
exprFromObject obj | just _ = Left "AstExpr type not a string"
exprFromObject obj | nothing = Left "AstExpr missing type"

{-# NON_TERMINATING #-}
statFromJSON (object obj) = statFromObject obj
statFromJSON _ = Left "AstStat not an object"

statFromObject obj with lookup type obj
statFromObject obj | just(string "AstStatLocal") with lookup vars obj | lookup values obj
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) with head(arr1) | head(arr2)
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(string x) | just(value) with exprFromJSON(value)
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(string x) | just(value) | Right M = Right (local (var x) ← M)
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(string x) | just(value) | Left err = Left err
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(string x) | nothing = Left "AstStatLocal empty values"
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | just(_) | _ = Left "AstStatLocal vars not a string array"
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(array arr2) | nothing | _ = Left "AstStatLocal empty vars"
statFromObject obj | just(string "AstStatLocal") | just(array arr1) | just(_) = Left "AstStatLocal values not an array"
statFromObject obj | just(string "AstStatLocal") | just(_) | just(_) = Left "AstStatLocal vars not an array"
statFromObject obj | just(string "AstStatLocal") | just(_) | nothing = Left "AstStatLocal missing values"
statFromObject obj | just(string "AstStatLocal") | nothing | _ = Left "AstStatLocal missing vars"
statFromObject obj | just(string "AstStatLocalFunction") with lookup name obj | lookup func obj
statFromObject obj | just(string "AstStatLocalFunction") | just (string f) | just value with exprFromJSON value
statFromObject obj | just(string "AstStatLocalFunction") | just (string f) | just value | Right (function⟨ x ⟩ B end) = Right (function f ⟨ x ⟩ B end)
statFromObject obj | just(string "AstStatLocalFunction") | just (string f) | just value | Left err = Left err
statFromObject obj | just(string "AstStatLocalFunction") | just _ | just _ | Right _ = Left "AstStatLocalFunction func is not an AstExprFunction"
statFromObject obj | just(string "AstStatLocalFunction") | just _ | just _ =  Left "AstStatLocalFunction name is not a string"
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
