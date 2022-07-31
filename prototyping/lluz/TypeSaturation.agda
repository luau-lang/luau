module Luau.TypeSaturation where

open import Luau.Type using (Type; _⇒_; _∩_; _∪_)
open import Luau.TypeNormalization using (_∪ⁿ_; _∩ⁿ_)

-- So, there's a problem with overloaded functions
-- (of the form (S_1 ⇒ T_1) ∩⋯∩ (S_n ⇒ T_n))
-- which is that it's not good enough to compare them
-- for subtyping by comparing all of their overloads.

-- For example (nil → nil) is a subtype of (number? → number?) ∩ (string? → string?)
-- but not a subtype of any of its overloads.

-- To fix this, we adapt the semantic subtyping algorithm for
-- function types, given in
-- https://www.irif.fr/~gc/papers/covcon-again.pdf and
-- https://pnwamk.github.io/sst-tutorial/

-- A function type is *intersection-saturated* if for any overloads
-- (S₁ ⇒ T₁) and (S₂ ⇒ T₂), there exists an overload which is a subtype
-- of ((S₁ ∩ S₂) ⇒ (T₁ ∩ T₂)).

-- A function type is *union-saturated* if for any overloads
-- (S₁ ⇒ T₁) and (S₂ ⇒ T₂), there exists an overload which is a subtype
-- of ((S₁ ∪ S₂) ⇒ (T₁ ∪ T₂)).

-- A function type is *saturated* if it is both intersection- and
-- union-saturated.

-- For example (number? → number?) ∩ (string? → string?)
-- is not saturated, but (number? → number?) ∩ (string? → string?) ∩ (nil → nil) ∩ ((number ∪ string)? → (number ∪ string)?)
-- is.

-- Saturated function types have the nice property that they can ber
-- compared by just comparing their overloads: F <: G whenever for any
-- overload of G, there is an overload os F which is a subtype of it.

-- Forunately every function type can be saturated!
_⋓_ : Type → Type → Type
(S₁ ⇒ T₁) ⋓ (S₂ ⇒ T₂) = (S₁ ∪ⁿ S₂) ⇒ (T₁ ∪ⁿ T₂)
(F₁ ∩ G₁) ⋓ F₂ = (F₁ ⋓ F₂) ∩ (G₁ ⋓ F₂)
F₁ ⋓ (F₂ ∩ G₂) = (F₁ ⋓ F₂) ∩ (F₁ ⋓ G₂)
F ⋓ G = F ∩ G

_⋒_ : Type → Type → Type
(S₁ ⇒ T₁) ⋒ (S₂ ⇒ T₂) = (S₁ ∩ⁿ S₂) ⇒ (T₁ ∩ⁿ T₂)
(F₁ ∩ G₁) ⋒ F₂ = (F₁ ⋒ F₂) ∩ (G₁ ⋒ F₂)
F₁ ⋒ (F₂ ∩ G₂) = (F₁ ⋒ F₂) ∩ (F₁ ⋒ G₂)
F ⋒ G = F ∩ G

_∩ᵘ_ : Type → Type → Type
F ∩ᵘ G = (F ∩ G) ∩ (F ⋓ G)

_∩ⁱ_ : Type → Type → Type
F ∩ⁱ G = (F ∩ G) ∩ (F ⋒ G)

∪-saturate : Type → Type
∪-saturate (F ∩ G) = (∪-saturate F ∩ᵘ ∪-saturate G)
∪-saturate F = F

∩-saturate : Type → Type
∩-saturate (F ∩ G) = (∩-saturate F ∩ⁱ ∩-saturate G)
∩-saturate F = F

saturate : Type → Type
saturate F = ∪-saturate (∩-saturate F)
