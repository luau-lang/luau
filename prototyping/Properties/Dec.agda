module Properties.Dec where

open import Properties.Contradiction using (¬)

data Dec(A : Set) : Set where
  yes : A → Dec A
  no : ¬ A → Dec A
