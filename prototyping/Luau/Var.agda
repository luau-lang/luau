module Luau.Var where

open import Agda.Builtin.Bool using (true; false)
open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.String using (String; primStringEquality)
open import Agda.Builtin.TrustMe using (primTrustMe)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Equality using (_≢_)

Var : Set
Var = String

_≡ⱽ_ : (a b : Var) → Dec (a ≡ b)
a ≡ⱽ b with primStringEquality a b
a ≡ⱽ b | false = no p where postulate p : (a ≢ b)
a ≡ⱽ b | true = yes primTrustMe
