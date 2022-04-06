module Luau.Type.ToString where

open import FFI.Data.String using (String; _++_)
open import Luau.Type using (Type; nil; _⇒_; never; unknown; number; boolean; string; _∪_; _∩_; normalizeOptional)

{-# TERMINATING #-}
typeToString : Type → String
typeToStringᵁ : Type → String
typeToStringᴵ : Type → String

typeToString nil = "nil"
typeToString (S ⇒ T) = "(" ++ (typeToString S) ++ ") -> " ++ (typeToString T)
typeToString never = "never"
typeToString unknown = "unknown"
typeToString number = "number"
typeToString boolean = "boolean"
typeToString string = "string"
typeToString (S ∪ T) with normalizeOptional(S ∪ T)
typeToString (S ∪ T) | ((S′ ⇒ T′) ∪ nil) = "(" ++ typeToString (S′ ⇒ T′) ++ ")?"
typeToString (S ∪ T) | (S′ ∪ nil) = typeToString S′ ++ "?"
typeToString (S ∪ T) | (S′ ∪ T′) = "(" ++ typeToStringᵁ (S ∪ T) ++ ")"
typeToString (S ∪ T) | T′ = typeToString T′
typeToString (S ∩ T) = "(" ++ typeToStringᴵ (S ∩ T) ++ ")"

typeToStringᵁ (S ∪ T) = (typeToStringᵁ S) ++ " | " ++ (typeToStringᵁ T)
typeToStringᵁ T = typeToString T

typeToStringᴵ (S ∩ T) = (typeToStringᴵ S) ++ " & " ++ (typeToStringᴵ T)
typeToStringᴵ T = typeToString T
