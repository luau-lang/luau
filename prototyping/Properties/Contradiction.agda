module Properties.Contradiction where

data ⊥ : Set where

¬ : Set → Set
¬ A = A → ⊥

CONTRADICTION : ∀ {A : Set} → ⊥ → A
CONTRADICTION ()
