module Luau.Syntax.FromJSON where

open import Luau.Syntax using (Type; Block; Expr; nil; return)

open import Agda.Builtin.String using (String)

open import FFI.Data.Aeson using (Value)
open import FFI.Data.Either using (Either; Left; Right)

exprFromJSON : Value → Either String Expr
blockFromJSON : Value → Either String Block

-- TODO: implement this!
exprFromJSON v = Left "Not implemented yet"

-- TODO: implement this!
blockFromJSON v = Left "Not implemented yet"
