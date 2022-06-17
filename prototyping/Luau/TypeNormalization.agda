module Luau.TypeNormalization where

open import Luau.Type using (Type; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_)

-- Operations on normalized types
_∪ᶠ_ : Type → Type → Type
_∪ⁿˢ_ : Type → Type → Type
_∩ⁿˢ_ : Type → Type → Type
_∪ⁿ_ : Type → Type → Type
_∩ⁿ_ : Type → Type → Type

-- Union of function types
(F₁ ∩ F₂) ∪ᶠ G = (F₁ ∪ᶠ G) ∩ (F₂ ∪ᶠ G)
F ∪ᶠ (G₁ ∩ G₂) = (F ∪ᶠ G₁) ∩ (F ∪ᶠ G₂)
(R ⇒ S) ∪ᶠ (T ⇒ U) = (R ∩ⁿ T) ⇒ (S ∪ⁿ U)
F ∪ᶠ G = F ∪ G

-- Union of normalized types
S ∪ⁿ (T₁ ∪ T₂) = (S ∪ⁿ T₁) ∪ T₂
S ∪ⁿ unknown = unknown
S ∪ⁿ never = S
never ∪ⁿ T = T
unknown ∪ⁿ T = unknown
(S₁ ∪ S₂) ∪ⁿ G = (S₁ ∪ⁿ G) ∪ S₂
F ∪ⁿ G = F ∪ᶠ G

-- Intersection of normalized types
S ∩ⁿ (T₁ ∪ T₂) = (S ∩ⁿ T₁) ∪ⁿˢ (S ∩ⁿˢ T₂)
S ∩ⁿ unknown = S
S ∩ⁿ never = never
(S₁ ∪ S₂) ∩ⁿ G = (S₁ ∩ⁿ G)
unknown ∩ⁿ G = G
never ∩ⁿ G = never
F ∩ⁿ G = F ∩ G

-- Intersection of normalized types with a scalar
(S₁ ∪ nil) ∩ⁿˢ nil = nil
(S₁ ∪ boolean) ∩ⁿˢ boolean = boolean
(S₁ ∪ number) ∩ⁿˢ number = number
(S₁ ∪ string) ∩ⁿˢ string = string
(S₁ ∪ S₂) ∩ⁿˢ T = S₁ ∩ⁿˢ T
unknown ∩ⁿˢ T = T
F ∩ⁿˢ T = never

-- Union of normalized types with an optional scalar
S ∪ⁿˢ never = S
unknown ∪ⁿˢ T = unknown
(S₁ ∪ nil) ∪ⁿˢ nil = S₁ ∪ nil
(S₁ ∪ boolean) ∪ⁿˢ boolean = S₁ ∪ boolean
(S₁ ∪ number) ∪ⁿˢ number = S₁ ∪ number
(S₁ ∪ string) ∪ⁿˢ string = S₁ ∪ string
(S₁ ∪ S₂) ∪ⁿˢ T = (S₁ ∪ⁿˢ T) ∪ S₂
F ∪ⁿˢ T = F ∪ T

-- Normalize!
normalize : Type → Type
normalize nil = never ∪ nil
normalize (S ⇒ T) = (normalize S ⇒ normalize T)
normalize never = never
normalize unknown = unknown
normalize boolean = never ∪ boolean
normalize number = never ∪ number
normalize string = never ∪ string
normalize (S ∪ T) = normalize S ∪ⁿ normalize T
normalize (S ∩ T) = normalize S ∩ⁿ normalize T
