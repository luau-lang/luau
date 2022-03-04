module Examples.Type where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.String using (_++_)
open import Luau.Type using (nil; _∪_; _∩_; _⇒_)
open import Luau.Type.ToString using (typeToString)

ex1 : typeToString(nil) ≡ "nil"
ex1 = refl

ex2 : typeToString(nil ⇒ nil) ≡ "(nil) -> nil"
ex2 = refl

ex3 : typeToString(nil ⇒ (nil ⇒ nil)) ≡ "(nil) -> (nil) -> nil"
ex3 = refl

ex4 : typeToString(nil ∪ (nil ⇒ (nil ⇒ nil))) ≡ "((nil) -> (nil) -> nil)?"
ex4 = refl

ex5 : typeToString(nil ⇒ ((nil ⇒ nil) ∪ nil)) ≡ "(nil) -> ((nil) -> nil)?"
ex5 = refl

ex6 : typeToString((nil ⇒ nil) ∪ (nil ⇒ (nil ⇒ nil))) ≡ "((nil) -> nil | (nil) -> (nil) -> nil)"
ex6 = refl

ex7 : typeToString((nil ⇒ nil) ∪ ((nil ⇒ (nil ⇒ nil)) ∪ nil)) ≡ "((nil) -> nil | (nil) -> (nil) -> nil)?"
ex7 = refl

