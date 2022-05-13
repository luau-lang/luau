module Luau.TypeSaturation where

open import Luau.Type using (Type; _⇒_; _∩_; _∪_)
open import Luau.TypeNormalization using (_∪ⁿ_; _∩ⁿ_; _⇒ⁿ_)

_⋓_ : Type → Type → Type
(S₁ ⇒ T₁) ⋓ (S₂ ⇒ T₂) = (S₁ ∪ⁿ S₂) ⇒ⁿ (T₁ ∪ⁿ T₂)
(F₁ ∩ G₁) ⋓ F₂ = (F₁ ⋓ F₂) ∩ (G₁ ⋓ F₂)
F₁ ⋓ (F₂ ∩ G₂) = (F₁ ⋓ F₂) ∩ (F₁ ⋓ G₂)
F ⋓ G = F ∩ G

_⋒_ : Type → Type → Type
(S₁ ⇒ T₁) ⋒ (S₂ ⇒ T₂) = (S₁ ∩ⁿ S₂) ⇒ⁿ (T₁ ∩ⁿ T₂)
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
