module Properties.Equality where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Properties.Contradiction using (¬)

sym : ∀ {A : Set} {a b : A} → (a ≡ b) → (b ≡ a)
sym refl = refl

trans : ∀ {A : Set} {a b c : A} → (a ≡ b) → (b ≡ c) → (a ≡ c)
trans refl refl = refl

cong : ∀ {A B : Set} {a b : A} (f : A → B) → (a ≡ b) → (f a ≡ f b)
cong f refl = refl

subst₁ : ∀ {A : Set} {a b : A} (F : A → Set) → (a ≡ b) → (F a) → (F b)
subst₁ F refl x = x

subst₂ : ∀ {A B : Set} {a b : A} {c d : B} (F : A → B → Set) → (a ≡ b) → (c ≡ d) → (F a c) → (F b d)
subst₂ F refl refl x = x

_≢_ : ∀ {A : Set} → A → A → Set
(a ≢ b) = ¬(a ≡ b)

