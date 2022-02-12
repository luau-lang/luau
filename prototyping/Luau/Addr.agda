module Luau.Addr where

open import Agda.Builtin.Bool using (true; false)
open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Nat using (Nat; _==_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.TrustMe using (primTrustMe)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Equality using (_≢_)

Addr : Set
Addr = Nat

_≡ᴬ_ : (a b : Addr) → Dec (a ≡ b)
a ≡ᴬ b with a == b
a ≡ᴬ b | false = no p where postulate p : (a ≢ b)
a ≡ᴬ b | true = yes primTrustMe

