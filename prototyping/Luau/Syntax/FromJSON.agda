module Luau.Syntax.FromJSON where

open import Luau.Syntax using (Type; Block; Stat ; Expr; nil; return; _∙; _∙_)

open import FFI.Data.Aeson using (Value; Array; Object; object; array; fromString; lookup)
open import FFI.Data.Bool using (true; false)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (nothing; just)
open import FFI.Data.String using (String; _++_)
open import FFI.Data.Vector using (head; tail; null; empty)

AstExprConstantNil = fromString "AstExprConstantNil"
AstStatReturn = fromString "AstStatReturn"

exprFromJSON : Value → Either String Expr
exprFromObject : Object → Either String Expr
statFromJSON : Value → Either String Stat
statFromObject : Object → Either String Stat
blockFromJSON : Value → Either String Block
blockFromArray : Array → Either String Block

exprFromJSON (object obj) = exprFromObject obj
exprFromJSON val = Left "Expr should be an object"

exprFromObject obj with lookup AstExprConstantNil obj
exprFromObject obj | just val = Right nil
exprFromObject obj | nothing = Left "Unsupported Expr"

{-# NON_TERMINATING #-}
statFromJSON (object obj) = statFromObject obj
statFromJSON _ = Left "Stat should be an object"

statFromObject obj with lookup AstStatReturn obj
statFromObject obj | just val with exprFromJSON val
statFromObject obj | just val | Left err = Left err
statFromObject obj | just val | Right exp = Right (return exp)
statFromObject obj | nothing = Left "Unsupported Stat"

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
