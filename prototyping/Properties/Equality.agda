module Properties.Equality where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Properties.Contradiction using (¬)

sym : ∀ {A : Set} {a b : A} → (a ≡ b) → (b ≡ a)
sym refl = refl

trans : ∀ {A : Set} {a b c : A} → (a ≡ b) → (b ≡ c) → (a ≡ c)
trans refl refl = refl

cong : ∀ {A B : Set} {a b : A} (f : A → B) → (a ≡ b) → (f a ≡ f b)
cong f refl = refl

_≢_ : ∀ {A : Set} → A → A → Set
(a ≢ b) = ¬(a ≡ b)

