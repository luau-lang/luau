module Luau.Syntax.FromJSON where

open import Luau.Syntax using (Type; Block; Expr; nil; return)

open import Agda.Builtin.String using (String)

open import FFI.Data.Aeson using (Value; Array; Object; object; array; fromString; lookup)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (nothing; just)
open import FFI.Data.Vector using (head; empty)

AstExprConstantNil = fromString "AstExprConstantNil"
AstStatReturn = fromString "AstStatReturn"

exprFromJSON : Value → Either String Expr
exprFromObject : Object → Either String Expr
blockFromJSON : Value → Either String Block
blockFromArray : Array → Either String Block
blockFromObject : Object → Array → Either String Block

exprFromJSON (object obj) = exprFromObject obj
exprFromJSON val = Left "Expr should be an object"

exprFromObject obj with lookup AstExprConstantNil obj
exprFromObject obj | just val = Right nil
exprFromObject obj | nothing = Left "Unsupported Expr"

blockFromJSON (object obj) = blockFromObject obj empty
blockFromJSON (array arr) = blockFromArray arr
blockFromJSON _ = Left "Block should be an object or array"

blockFromArray arr with head arr
blockFromArray arr | nothing = Right (return nil)
blockFromArray arr | just (object obj) = blockFromObject obj arr
blockFromArray arr | just (x) = Left "Stat should be an object"

blockFromObject obj arr with lookup AstStatReturn obj
blockFromObject obj arr | just val with exprFromJSON val
blockFromObject obj arr | just val | Left err = Left err
blockFromObject obj arr | just val | Right exp = Right (return exp)
blockFromObject obj arr | nothing = Left "Unsupported Stat"


