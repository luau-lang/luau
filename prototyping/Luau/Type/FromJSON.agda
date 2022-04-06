{-# OPTIONS --rewriting #-}

module Luau.Type.FromJSON where

open import Luau.Type using (Type; nil; _⇒_; _∪_; _∩_; unknown; never; number; string)

open import Agda.Builtin.List using (List; _∷_; [])
open import Agda.Builtin.Bool using (true; false)

open import FFI.Data.Aeson using (Value; Array; Object; object; array; string; fromString; lookup)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (Maybe; nothing; just)
open import FFI.Data.String using (String; _++_)
open import FFI.Data.Vector using (head; tail; null; empty)

name = fromString "name"
type = fromString "type"
argTypes = fromString "argTypes"
returnTypes = fromString "returnTypes"
types = fromString "types"

{-# TERMINATING #-}
typeFromJSON : Value → Either String Type
compoundFromArray : (Type → Type → Type) → Array → Either String Type

typeFromJSON (object o) with lookup type o
typeFromJSON (object o) | just (string "AstTypeFunction") with lookup argTypes o | lookup returnTypes o
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) with lookup types argsSet | lookup types retsSet
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just (array args) | just (array rets) with head args | head rets
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just (array args) | just (array rets) | just argValue | just retValue with typeFromJSON argValue | typeFromJSON retValue
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just (array args) | just (array rets) | just argValue | just retValue | Right arg | Right ret = Right (arg ⇒ ret)
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just (array args) | just (array rets) | just argValue | just retValue | Left err | _ = Left err
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just (array args) | just (array rets) | just argValue | just retValue | _ | Left err = Left err
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just (array args) | just (array rets) | _ | nothing = Left "No return type"
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just (array args) | just (array rets) | nothing | _ = Left "No argument type"
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | just _ | _ = Left "argTypes.types is not an array"
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | _ | just _ = Left "retTypes.types is not an array"
typeFromJSON (object o) | just (string "AstTypeFunction") | just (object argsSet) | just (object retsSet) | nothing | _ = Left "argTypes.types does not exist"
typeFromJSON (object o) | just (string "AstTypeFunction") | _ | just _ = Left "argTypes is not an object"
typeFromJSON (object o) | just (string "AstTypeFunction") | just _ | _ = Left "returnTypes is not an object"
typeFromJSON (object o) | just (string "AstTypeFunction") | nothing | nothing = Left "Missing argTypes and returnTypes"

typeFromJSON (object o) | just (string "AstTypeReference") with lookup name o
typeFromJSON (object o) | just (string "AstTypeReference") | just (string "nil") = Right nil
typeFromJSON (object o) | just (string "AstTypeReference") | just (string "any") = Right unknown -- not quite right
typeFromJSON (object o) | just (string "AstTypeReference") | just (string "unknown") = Right unknown
typeFromJSON (object o) | just (string "AstTypeReference") | just (string "never") = Right never
typeFromJSON (object o) | just (string "AstTypeReference") | just (string "number") = Right number
typeFromJSON (object o) | just (string "AstTypeReference") | just (string "string") = Right string
typeFromJSON (object o) | just (string "AstTypeReference") | _ = Left "Unknown referenced type"

typeFromJSON (object o) | just (string "AstTypeUnion") with lookup types o
typeFromJSON (object o) | just (string "AstTypeUnion") | just (array types) = compoundFromArray _∪_ types
typeFromJSON (object o) | just (string "AstTypeUnion") | _ = Left "`types` field must be an array"

typeFromJSON (object o) | just (string "AstTypeIntersection") with lookup types o
typeFromJSON (object o) | just (string "AstTypeIntersection") | just (array types) = compoundFromArray _∩_ types
typeFromJSON (object o) | just (string "AstTypeIntersection") | _ = Left "`types` field must be an array"

typeFromJSON (object o) | just (string ty) = Left ("Unsupported type " ++ ty)
typeFromJSON (object o) | just _ = Left "`type` field must be a string"
typeFromJSON (object o) | nothing = Left "No `type` field"
typeFromJSON _ = Left "Unsupported JSON type"

compoundFromArray ctor ts with head ts | tail ts
compoundFromArray ctor ts | just hd | tl with null tl
compoundFromArray ctor ts | just hd | tl | true = typeFromJSON hd
compoundFromArray ctor ts | just hd | tl | false with typeFromJSON hd | compoundFromArray ctor tl
compoundFromArray ctor ts | just hd | tl | false | Right hdTy | Right tlTy = Right (ctor hdTy tlTy)
compoundFromArray ctor ts | just hd | tl | false | Left err | _ = Left err
compoundFromArray ctor ts | just hd | tl | false | _ | Left Err = Left Err
compoundFromArray ctor ts | nothing | empty = Left "Empty types array?"
