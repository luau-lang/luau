module Utility.Bool where

open import Agda.Builtin.Bool using (Bool; true; false)

not : Bool → Bool
not false = true
not true = false

_or_ : Bool → Bool → Bool
true or _ = true
_ or true = true
_ or _ = false

_and_ : Bool → Bool → Bool
true and true = true
_ and _ = false
