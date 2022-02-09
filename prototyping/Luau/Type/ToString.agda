module Luau.Type.ToString where

open import FFI.Data.String using (String; _++_)
open import Luau.Type using (Type; nil; _⇒_; none; any; _∪_; _∩_)

typeToString : Type → String
typeToString nil = "nil"
typeToString (S ⇒ T) = "(" ++ (typeToString S) ++ ") -> " ++ (typeToString T)
typeToString none = "none"
typeToString any = "any"
typeToString (S ∪ T) = (typeToString S) ++ " | " ++ (typeToString T)
typeToString (S ∩ T) = (typeToString S) ++ " & " ++ (typeToString T)

