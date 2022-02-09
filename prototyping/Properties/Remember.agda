module Properties.Remember where

open import Agda.Builtin.Equality using (_≡_; refl)

data Remember {A : Set} (a : A) : Set where
  _,_ : ∀ b → (a ≡ b) → Remember(a)
  
remember : ∀ {A} (a : A) → Remember(a)
remember a = (a , refl)
