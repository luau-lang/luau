module Properties.Dec where

data ⊥ : Set where

data Dec(A : Set) : Set where
  yes : A → Dec A
  no : (A → ⊥) → Dec A

