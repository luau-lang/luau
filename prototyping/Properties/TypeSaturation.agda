{-# OPTIONS --rewriting #-}

module Properties.TypeSaturation where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right)
open import Luau.Subtyping using (Tree; Language; ¬Language; _<:_; _≮:_; witness; scalar; function; function-err; function-ok; function-ok₁; function-ok₂; scalar-function; _,_)
open import Luau.Type using (Type; _⇒_; _∩_; _∪_; never; unknown)
open import Luau.TypeNormalization using (_⇒ⁿ_; _∩ⁿ_; _∪ⁿ_)
open import Luau.TypeSaturation using (_⋓_; _⋒_; _∩ᵘ_; _∩ⁱ_; ∪-saturate; ∩-saturate; saturate)
open import Properties.Subtyping using (dec-language; language-comp; <:-impl-⊇; <:-refl; <:-trans; <:-trans-≮:; <:-impl-¬≮: ; <:-function; <:-union; <:-∪-symm; <:-∪-left; <:-∪-right; <:-∪-lub; <:-∪-assocl; <:-∪-assocr; <:-intersect; <:-∩-symm; <:-∩-left; <:-∩-right; <:-∩-glb; ≮:-function-left; ≮:-function-right; <:-∩-assocl; <:-∩-assocr; ∩-<:-∪; <:-∩-distl-∪; ∩-distl-∪-<:; <:-∩-distr-∪; ∩-distr-∪-<:)
open import Properties.TypeNormalization using (FunType; function; _⇒_; _∩_; _∪_; never; unknown; inhabitant; inhabited; function-top; normal-⇒ⁿ; normal-∪ⁿ; normal-∩ⁿ; normalⁱ; <:-tgtⁿ; ∪ⁿ-<:-∪; ∪-<:-∪ⁿ)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.Functions using (_∘_)

-- Overload F (S ⇒ T) when (S ⇒ T) is an overload of F
data Overload : Type → Type → Set where

   here : ∀ {S T} → Overload (S ⇒ T) (S ⇒ T)
   left : ∀ {S T F G} → Overload F (S ⇒ T) → Overload (F ∩ G) (S ⇒ T)
   right : ∀ {S T F G} → Overload G (S ⇒ T) → Overload (F ∩ G) (S ⇒ T)

-- F <:ᵒ (S ⇒ T) when (S ⇒ T) is a supertype of an overload of F
data _<:ᵒ_ : Type → Type → Set where

  defn : ∀ {F R S T U} →

    Overload F (R ⇒ S) →
    T <: R →
    S <: U →
    ---------------------
    F <:ᵒ (T ⇒ U)

-- Saturated F whenever
-- * if F has overloads (R ⇒ S) and (T ⇒ U) then F has an overload which is a subtype of ((R ∩ T) ⇒ (S ∩ U))
-- * ditto union

data Saturated (F : Type) : Set where

  defn : 

    (∀ {R S T U} → Overload F (R ⇒ S) → Overload F (T ⇒ U) → F <:ᵒ ((R ∩ T) ⇒ (S ∩ U))) →
    (∀ {R S T U} → Overload F (R ⇒ S) → Overload F (T ⇒ U) → F <:ᵒ ((R ∪ T) ⇒ (S ∪ U))) →
    -----------
    Saturated F

-- Saturated functions are interesting because they have a decision procedure
-- for subtyping.

-- Saturation preserves normalization
normal-⋒ : ∀ {F G} → FunType F → FunType G → FunType (F ⋒ G)
normal-⋒ function function = function
normal-⋒ function (T ⇒ U) = normal-⇒ⁿ (normal-∩ⁿ never (normalⁱ T)) (normal-∩ⁿ unknown U)
normal-⋒ function (G ∩ H) = normal-⋒ function G ∩ normal-⋒ function H
normal-⋒ (R ⇒ S) function = normal-⇒ⁿ (normal-∩ⁿ (normalⁱ R) never) (normal-∩ⁿ S unknown)
normal-⋒ (R ⇒ S) (T ⇒ U) = normal-⇒ⁿ (normal-∩ⁿ (normalⁱ R) (normalⁱ T)) (normal-∩ⁿ S U)
normal-⋒ (R ⇒ S) (G ∩ H) = normal-⋒ (R ⇒ S) G ∩ normal-⋒ (R ⇒ S) H
normal-⋒ (E ∩ F) G = normal-⋒ E G ∩ normal-⋒ F G

normal-⋓ : ∀ {F G} → FunType F → FunType G → FunType (F ⋓ G)
normal-⋓ function function = function
normal-⋓ function (T ⇒ U) = normal-⇒ⁿ (normal-∪ⁿ never (normalⁱ T)) (normal-∪ⁿ unknown U)
normal-⋓ function (G ∩ H) = normal-⋓ function G ∩ normal-⋓ function H
normal-⋓ (R ⇒ S) function = normal-⇒ⁿ (normal-∪ⁿ (normalⁱ R) never) (normal-∪ⁿ S unknown)
normal-⋓ (R ⇒ S) (T ⇒ U) = normal-⇒ⁿ (normal-∪ⁿ (normalⁱ R) (normalⁱ T)) (normal-∪ⁿ S U)
normal-⋓ (R ⇒ S) (G ∩ H) = normal-⋓ (R ⇒ S) G ∩ normal-⋓ (R ⇒ S) H
normal-⋓ (E ∩ F) G = normal-⋓ E G ∩ normal-⋓ F G

normal-∩-saturate : ∀ {F} → FunType F → FunType (∩-saturate F)
normal-∩-saturate function = function
normal-∩-saturate (S ⇒ T) = S ⇒ T
normal-∩-saturate (F ∩ G) = (normal-∩-saturate F ∩ normal-∩-saturate G) ∩ normal-⋒ (normal-∩-saturate F) (normal-∩-saturate G)

normal-∪-saturate : ∀ {F} → FunType F → FunType (∪-saturate F)
normal-∪-saturate function = function
normal-∪-saturate (S ⇒ T) = S ⇒ T
normal-∪-saturate (F ∩ G) = (normal-∪-saturate F ∩ normal-∪-saturate G) ∩ normal-⋓ (normal-∪-saturate F) (normal-∪-saturate G)

normal-saturate : ∀ {F} → FunType F → FunType (saturate F)
normal-saturate F = normal-∪-saturate (normal-∩-saturate F)

-- Order types by overloading
-- F ⊆ᵒ G whenever every overload of F is an overload of G
_⊆ᵒ_ : Type → Type → Set
F ⊆ᵒ G = ∀ {S T} → Overload F (S ⇒ T) → Overload G (S ⇒ T)

-- F ⊂:ᵒ G whenever every overload of F is a subtype of an overload of G
_⊂:ᵒ_ : Type → Type → Set
F ⊂:ᵒ G = ∀ {S T} → Overload F (S ⇒ T) → G <:ᵒ (S ⇒ T)

-- Properties of <:ᵒ
⋒-⋓-cl-impl-sat : ∀ {F} → (F ⋒ F) ⊂:ᵒ F → (F ⋓ F) ⊂:ᵒ F → Saturated F
⋒-⋓-cl-impl-sat = {!!}

<:ᵒ-refl : ∀ {S T} → (S ⇒ T) <:ᵒ (S ⇒ T)
<:ᵒ-refl = defn here <:-refl <:-refl

<:ᵒ-left : ∀ {F G S T} → F <:ᵒ (S ⇒ T) → (F ∩ G) <:ᵒ (S ⇒ T)
<:ᵒ-left = {!!}

<:ᵒ-right : ∀ {F G S T} → G <:ᵒ (S ⇒ T) → (F ∩ G) <:ᵒ (S ⇒ T)
<:ᵒ-right = {!!}

<:ᵒ-ov : ∀ {F S T} → Overload F (S ⇒ T) → F <:ᵒ (S ⇒ T)
<:ᵒ-ov o = defn o <:-refl <:-refl

<:ᵒ-trans-<: : ∀ {F S T S′ T′} → F <:ᵒ (S ⇒ T) → (S′ <: S) → (T <: T′) → F <:ᵒ (S′ ⇒ T′)
<:ᵒ-trans-<: = {!!}

ov-language : ∀ {F t} → FunType F → (∀ {S T} → Overload F (S ⇒ T) → Language (S ⇒ T) t) → Language F t
ov-language function p = p here
ov-language (S ⇒ T) p = p here
ov-language (F ∩ G) p = (ov-language F (p ∘ left) , ov-language G (p ∘ right))

ov-<: : ∀ {F R S T U} → Overload F (R ⇒ S) → ((R ⇒ S) <: (T ⇒ U)) → F <: (T ⇒ U)
ov-<: here p = p
ov-<: (left o) p = <:-trans <:-∩-left (ov-<: o p)
ov-<: (right o) p = <:-trans <:-∩-right (ov-<: o p)

⊆ᵒ-left : ∀ {F G} → F ⊆ᵒ (F ∩ G)
⊆ᵒ-left = left

⊆ᵒ-right : ∀ {F G} → G ⊆ᵒ (F ∩ G)
⊆ᵒ-right = right

⋒-cl-∩ : ∀ {F} → (F ⋒ F) ⊂:ᵒ F → ∀ {R S T U} → Overload F (R ⇒ S) → Overload F (T ⇒ U) → F <:ᵒ ((R ∩ T) ⇒ (S ∩ U))
⋒-cl-∩ = {!!}

⋓-cl-∪ : ∀ {F} → (F ⋓ F) ⊂:ᵒ F → ∀ {R S T U} → Overload F (R ⇒ S) → Overload F (T ⇒ U) → F <:ᵒ ((R ∪ T) ⇒ (S ∪ U))
⋓-cl-∪ = {!!}

-- The overloads of (F ⋓ G) are unions of overloads from F and G
data ⋓-Overload F G : Type → Set where

  defn : ∀ {R S T U} →

    Overload F (R ⇒ S) →
    Overload G (T ⇒ U) →
    ---------------------------
    ⋓-Overload F G ((R ∪ T) ⇒ (S ∪ U))

⋓-∪-overload : ∀ F G {S T} → Overload (F ⋓ G) (S ⇒ T) → ⋓-Overload F G (S ⇒ T)
⋓-∪-overload = {!!}

-- Properties of ⊂:ᵒ 
⊂:ᵒ-refl : ∀ {F} → (F ⊂:ᵒ F)
⊂:ᵒ-refl o = defn o (λ t z → z) (λ t z → z)

⊂:ᵒ-trans : ∀ {F G H} → (F ⊂:ᵒ G) → (G ⊂:ᵒ H) → (F ⊂:ᵒ H)  
⊂:ᵒ-trans = {!!}

⊂:ᵒ-left : ∀ {F G H} → (F ⊂:ᵒ G) → (F ⊂:ᵒ (G ∩ H))  
⊂:ᵒ-left = {!!}

⊂:ᵒ-right : ∀ {F G H} → (F ⊂:ᵒ H) → (F ⊂:ᵒ (G ∩ H))  
⊂:ᵒ-right = {!!}

⊂:ᵒ-lub : ∀ {F G H} → (F ⊂:ᵒ H) → (G ⊂:ᵒ H) → ((F ∩ G) ⊂:ᵒ H)  
⊂:ᵒ-lub = {!!}

⊂:ᵒ-⋓-symm : ∀ {F G} → ((F ⋓ G) ⊂:ᵒ (G ⋓ F))
⊂:ᵒ-⋓-symm = {!!}

⊂:ᵒ-⋓-assocl : ∀ {F G H} → (F ⋓ (G ⋓ H)) ⊂:ᵒ ((F ⋓ G) ⋓ H)
⊂:ᵒ-⋓-assocl = {!!}

⊂:ᵒ-⋓-assocr : ∀ {F G H} → ((F ⋓ G) ⋓ H) ⊂:ᵒ (F ⋓ (G ⋓ H))
⊂:ᵒ-⋓-assocr = {!!}

⊂:ᵒ-⋓-redist : ∀ {E F G H} → ((E ⋓ F) ⋓ (G ⋓ H)) ⊂:ᵒ ((E ⋓ G) ⋓ (F ⋓ H))
⊂:ᵒ-⋓-redist = {!!}

⊂:ᵒ-⋓-dist-∩ : ∀ F G H → (F ⋓ (G ∩ H)) ⊂:ᵒ ((F ⋓ G) ∩ (F ⋓ H))
⊂:ᵒ-⋓-dist-∩ = {!!}

⊂:ᵒ-⋓-dist-⋒ : ∀ {F G H} → (F ⋓ (G ⋒ H)) ⊂:ᵒ ((F ⋓ G) ⋒ (F ⋓ H))
⊂:ᵒ-⋓-dist-⋒ = {!!}

⊂:ᵒ-⋓ : ∀ {E F G H} → (E ⊂:ᵒ F) → (G ⊂:ᵒ H) → ((E ⋓ G) ⊂:ᵒ (F ⋓ H))
⊂:ᵒ-⋓ = {!!}

⊂:ᵒ-⋒ : ∀ {E F G H} → (E ⊂:ᵒ F) → (G ⊂:ᵒ H) → ((E ⋒ G) ⊂:ᵒ (F ⋒ H))
⊂:ᵒ-⋒ = {!!}

-- Every function can be ∪-saturated!
∩ᵘ-∪-saturated : ∀ {F G} → (F ⋓ F) ⊂:ᵒ F → (G ⋓ G) ⊂:ᵒ G → ((F ∩ᵘ G) ⋓ (F ∩ᵘ G)) ⊂:ᵒ (F ∩ᵘ G)
∩ᵘ-∪-saturated {F} {G} Fˢ Gˢ = ⊂:ᵒ-trans
  (⊂:ᵒ-⋓-dist-∩ (F ∩ᵘ G) (F ∩ G) (F ⋓ G))
  (⊂:ᵒ-lub (⊂:ᵒ-lub (⊂:ᵒ-lub
    (⊂:ᵒ-trans (⊂:ᵒ-⋓-dist-∩ F F G) (⊂:ᵒ-lub (⊂:ᵒ-trans Fˢ (⊂:ᵒ-left (⊂:ᵒ-left ⊂:ᵒ-refl))) (⊂:ᵒ-right ⊂:ᵒ-refl)))
    (⊂:ᵒ-trans (⊂:ᵒ-⋓-dist-∩ G F G) (⊂:ᵒ-lub (⊂:ᵒ-right (⊂:ᵒ-⋓-symm {G})) (⊂:ᵒ-trans Gˢ (⊂:ᵒ-left (⊂:ᵒ-right ⊂:ᵒ-refl))))))
    (⊂:ᵒ-trans (⊂:ᵒ-⋓-dist-∩ (F ⋓ G) F G) (⊂:ᵒ-lub (⊂:ᵒ-right (⊂:ᵒ-trans (⊂:ᵒ-⋓-symm {F ⋓ G}) (⊂:ᵒ-trans (⊂:ᵒ-⋓-assocl {F}) (⊂:ᵒ-⋓ Fˢ ⊂:ᵒ-refl)))) (⊂:ᵒ-trans (⊂:ᵒ-⋓-assocr {F}) (⊂:ᵒ-right (⊂:ᵒ-⋓ (⊂:ᵒ-refl {F}) Gˢ))))))
    (⊂:ᵒ-lub (⊂:ᵒ-lub
      (⊂:ᵒ-trans (⊂:ᵒ-⋓-assocl {F}) (⊂:ᵒ-right (⊂:ᵒ-⋓ Fˢ ⊂:ᵒ-refl)))
      (⊂:ᵒ-trans (⊂:ᵒ-⋓-symm {G}) (⊂:ᵒ-trans (⊂:ᵒ-⋓-assocr {F}) (⊂:ᵒ-right (⊂:ᵒ-⋓ (⊂:ᵒ-refl {F}) Gˢ)))))
      (⊂:ᵒ-trans (⊂:ᵒ-⋓-redist {F}) (⊂:ᵒ-right (⊂:ᵒ-⋓ Fˢ Gˢ)))))

⊆ᵒ-∪-sat : ∀ {F} → F ⊆ᵒ ∪-saturate F
⊆ᵒ-∪-sat here = here
⊆ᵒ-∪-sat (left o) = left (left (⊆ᵒ-∪-sat o))
⊆ᵒ-∪-sat (right o) = left (right (⊆ᵒ-∪-sat o))

∪-∪-saturated : ∀ {F} → (FunType F) → (∪-saturate F ⋓ ∪-saturate F) ⊂:ᵒ ∪-saturate F
∪-∪-saturated function here = <:ᵒ-refl
∪-∪-saturated (Sⁱ ⇒ Tⁿ) here = defn here (<:-trans (∪ⁿ-<:-∪ (normalⁱ Sⁱ) (normalⁱ Sⁱ)) (<:-∪-lub <:-refl <:-refl)) (<:-tgtⁿ (<:-trans <:-∪-left (∪-<:-∪ⁿ Tⁿ Tⁿ)))
∪-∪-saturated (Fᶠ ∩ Gᶠ) o = ∩ᵘ-∪-saturated (∪-∪-saturated Fᶠ) (∪-∪-saturated Gᶠ) o

-- ∩-saturate is ⋓-closed
∪-saturated : ∀ {F} → (FunType F) → (saturate F ⋓ saturate F) ⊂:ᵒ saturate F
∪-saturated F = ∪-∪-saturated (normal-∩-saturate F)

-- ∩-saturate is ⋒-closed
ov-⋒-∩ : ∀ {F G R S T U} → Overload F (R ⇒ S) → Overload G (T ⇒ U) → Overload (F ⋒ G) ((R ∩ T) ⇒ (S ∩ U))
ov-⋒-∩ = {!!}

∩-∩-saturated : ∀ {F} → (FunType F) → (∩-saturate F ⋒ ∩-saturate F) ⊂:ᵒ ∩-saturate F
∩-∩-saturated F = {!!}

-- An inductive presentation of the ⋒-closure of a type
data ⋒-Overload F G : Type → Set where

  defn : ∀ {R S T U} →

    Overload F (R ⇒ S) →
    Overload G (T ⇒ U) →
    ---------------------------
    ⋒-Overload F G ((R ∩ T) ⇒ (S ∩ U))

⋒-overload : ∀ F G {S T} → Overload (F ⋒ G) (S ⇒ T) → ⋒-Overload F G (S ⇒ T)
⋒-overload = {!!}

-- An inductive presentation of the ⋓-closure of a type
data ⋓-Closure F : Type → Set where

  ov : ∀ {S T} →
  
    Overload F (S ⇒ T) →
    -------------------
    ⋓-Closure F (S ⇒ T)

  union : ∀ {R S T U} →

    ⋓-Closure F (R ⇒ S) →
    ⋓-Closure F (T ⇒ U) →
    -------------------------------
    ⋓-Closure F ((R ∪ T) ⇒ (S ∪ U))

data ⋓-Closure-<: F : Type → Set where
 
 defn : ∀ {R S T U} →

    ⋓-Closure F (R ⇒ S) →
    T <: R →
    S <: U →
    ---------------------
    ⋓-Closure-<: F (T ⇒ U)

⋓-cl-⊆ᵒ : ∀ {F G S T} → (F ⊆ᵒ G) → ⋓-Closure F (S ⇒ T) → ⋓-Closure G (S ⇒ T)
⋓-cl-⊆ᵒ p (ov o) = ov (p o)
⋓-cl-⊆ᵒ p (union c d) = union (⋓-cl-⊆ᵒ p c) (⋓-cl-⊆ᵒ p d)

∪-sat-closure : ∀ {F S T} → FunType F → Overload (∪-saturate F) (S ⇒ T) → ⋓-Closure F (S ⇒ T)
∪-sat-closure function here = ov here
∪-sat-closure (S ⇒ T) here = ov here
∪-sat-closure (Fᶠ ∩ Gᶠ) (left (left o)) = ⋓-cl-⊆ᵒ ⊆ᵒ-left (∪-sat-closure Fᶠ o)
∪-sat-closure (Fᶠ ∩ Gᶠ) (left (right o)) = ⋓-cl-⊆ᵒ ⊆ᵒ-right (∪-sat-closure Gᶠ o)
∪-sat-closure {F ∩ G} (Fᶠ ∩ Gᶠ) (right o) with ⋓-∪-overload (∪-saturate F) (∪-saturate G) o
∪-sat-closure (Fᶠ ∩ Gᶠ) (right o) | defn p q = union (⋓-cl-⊆ᵒ ⊆ᵒ-left (∪-sat-closure Fᶠ p)) (⋓-cl-⊆ᵒ ⊆ᵒ-right (∪-sat-closure Gᶠ q))

closure-∪-sat-<:ᵒ : ∀ {F S T} → (FunType F) → ⋓-Closure F (S ⇒ T) → (∪-saturate F) <:ᵒ (S ⇒ T)
closure-∪-sat-<:ᵒ Fᶠ (ov o) = <:ᵒ-ov (⊆ᵒ-∪-sat o)
closure-∪-sat-<:ᵒ Fᶠ (union c d) with closure-∪-sat-<:ᵒ Fᶠ c | closure-∪-sat-<:ᵒ Fᶠ d
closure-∪-sat-<:ᵒ Fᶠ (union c d) | defn o o₁ o₂ | defn p p₁ p₂ = <:ᵒ-trans-<: (⋓-cl-∪ (∪-∪-saturated Fᶠ) o p) (<:-union o₁ p₁) (<:-union o₂ p₂)

∪-closure-<:ᵒ : ∀ {F S T} → F <:ᵒ (S ⇒ T) → ⋓-Closure-<: F (S ⇒ T)
∪-closure-<:ᵒ (defn o p q) = defn (ov o) p q

∪-closure-∩ : ∀ {F R S T U} → (FunType F) → (F ⋒ F) ⊂:ᵒ F → ⋓-Closure F (R ⇒ S) → ⋓-Closure F (T ⇒ U) → ⋓-Closure-<: F ((R ∩ T) ⇒ (S ∩ U))
∪-closure-∩ Fᶠ p (ov n) (ov o) = ∪-closure-<:ᵒ (p (ov-⋒-∩ n o))
∪-closure-∩ Fᶠ p c (union d d₁) with ∪-closure-∩ Fᶠ p c d | ∪-closure-∩ Fᶠ p c d₁ 
∪-closure-∩ Fᶠ p c (union d d₁) | defn e e₁ e₂ | defn f f₁ f₂ = defn (union e f) (<:-trans <:-∩-distl-∪ (<:-union e₁ f₁)) (<:-trans (<:-union e₂ f₂) ∩-distl-∪-<:)
∪-closure-∩ Fᶠ p (union c c₁) d with ∪-closure-∩ Fᶠ p c d | ∪-closure-∩ Fᶠ p c₁ d
∪-closure-∩ Fᶠ p (union c c₁) d | defn e e₁ e₂ | defn f f₁ f₂ = defn (union e f) (<:-trans <:-∩-distr-∪ (<:-union e₁ f₁)) (<:-trans (<:-union e₂ f₂) ∩-distr-∪-<:)

-- ∪-saturate preserves ⋒-closure
∪-∩-saturated : ∀ {F} → (FunType F) → (F ⋒ F) ⊂:ᵒ F → (∪-saturate F ⋒ ∪-saturate F) ⊂:ᵒ ∪-saturate F
∪-∩-saturated {F} Fᶠ p o with ⋒-overload (∪-saturate F) (∪-saturate F) o
∪-∩-saturated {F} Fᶠ p o | defn o₁ o₂ with ∪-sat-closure Fᶠ o₁ | ∪-sat-closure Fᶠ o₂ 
∪-∩-saturated {F} Fᶠ p o | defn o₁ o₂ | c₁ | c₂ with ∪-closure-∩ Fᶠ p c₁ c₂
∪-∩-saturated {F} Fᶠ p o | defn o₁ o₂ | c₁ | c₂ | defn d q r = <:ᵒ-trans-<: (closure-∪-sat-<:ᵒ Fᶠ d) q r

-- so saturate is ⋒-closed
∩-saturated : ∀ {F} → (FunType F) → (saturate F ⋒ saturate F) ⊂:ᵒ saturate F
∩-saturated F = ∪-∩-saturated (normal-∩-saturate F) (∩-∩-saturated F)

-- Every function type can be saturated!
saturated : ∀ {F} → (FunType F) → Saturated (saturate F)
saturated F = ⋒-⋓-cl-impl-sat (∩-saturated F) (∪-saturated F)

-- Subtyping is decidable on saturated normalized types

dec-<:-overloads : ∀ {F S T} → FunType F → FunType (S ⇒ T) → Saturated F →
  (∀ {S′ T′} → (Overload F (S′ ⇒ T′)) → Either (S ≮: S′) (S <: S′)) →
  (∀ {S′ T′} → (Overload F (S′ ⇒ T′)) → Either (T′ ≮: T) (T′ <: T)) →
  Either (F ≮: (S ⇒ T)) (F <: (S ⇒ T))
dec-<:-overloads {F} {S} {T} Fᶠ function _ _ _ = Right (function-top Fᶠ)
dec-<:-overloads {F} {S} {T} Fᶠ (Sⁱ ⇒ Tⁿ) (defn sat-∩ sat-∪) dec-src dec-tgt = result (top Fᶠ (λ o → o)) (bot Fᶠ (λ o → o)) where

  data Top G : Set where

    defn : ∀ Sᵗ Tᵗ →

      Overload F (Sᵗ ⇒ Tᵗ) →
      (∀ {S′ T′} → Overload G (S′ ⇒ T′) → (S′ <: Sᵗ)) →
      -------------
      Top G

  data Bot G : Set where

    defn : ∀ Sᵇ Tᵇ →

      Overload F (Sᵇ ⇒ Tᵇ) →
      (∀ {S′ T′} → Overload G (S′ ⇒ T′) → (Tᵇ <: T′)) →
      -------------
      Bot G

  top : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → Top G
  top {S′ ⇒ T′} _ G⊆F = defn S′ T′ (G⊆F here) (λ { here → <:-refl })
  top (Gᶠ ∩ Hᶠ) G⊆F with top Gᶠ (G⊆F ∘ left) | top Hᶠ (G⊆F ∘ right)
  top (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵗ Sᵗ p p₁ | defn Tᵗ Uᵗ q q₁ with sat-∪ p q
  top (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵗ Sᵗ p p₁ | defn Tᵗ Uᵗ q q₁ | defn n r r₁ = defn _ _ n
    (λ { (left o) → <:-trans (<:-trans (p₁ o) <:-∪-left) r ; (right o) → <:-trans (<:-trans (q₁ o) <:-∪-right) r })

  bot : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → Bot G
  bot {S′ ⇒ T′} _ G⊆F = defn S′ T′ (G⊆F here) (λ { here → <:-refl })
  bot (Gᶠ ∩ Hᶠ) G⊆F with bot Gᶠ (G⊆F ∘ left) | bot Hᶠ (G⊆F ∘ right)
  bot (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵇ Sᵇ p p₁ | defn Tᵇ Uᵇ q q₁ with sat-∩ p q
  bot (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵇ Sᵇ p p₁ | defn Tᵇ Uᵇ q q₁ | defn n r r₁ = defn _ _ n
    (λ { (left o) → <:-trans (<:-trans r₁ <:-∩-left) (p₁ o) ; (right o) → <:-trans (<:-trans r₁ <:-∩-right) (q₁ o) })

  result : Top F → Bot F → Either (F ≮: (S ⇒ T)) (F <: (S ⇒ T))
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) (defn Sᵇ Tᵇ oᵇ tgtᵇ) with dec-src oᵗ | dec-tgt oᵇ
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) (defn Sᵇ Tᵇ oᵇ tgtᵇ) | Left (witness s Ss ¬Sᵗs) | _ = Left (witness (function-err s) (ov-language Fᶠ (λ o → function-err (<:-impl-⊇ (srcᵗ o) s ¬Sᵗs))) (function-err Ss))
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) (defn Sᵇ Tᵇ oᵇ tgtᵇ) | _ | Left (witness t Tᵇt ¬Tt) = Left (witness (function-ok (inhabitant Sⁱ) t) (ov-language Fᶠ (λ o → function-ok₂ (tgtᵇ o t Tᵇt))) (function-ok (inhabited Sⁱ) ¬Tt))
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) (defn Sᵇ Tᵇ oᵇ tgtᵇ) | Right S<:Sᵗ | Right Tᵇ<:T = result₀ (largest Fᶠ (λ o → o)) where

    data LargestSrc (G : Type) : Set where

      defn : ∀ S₀ T₀ →

        Overload F (S₀ ⇒ T₀) →
        T₀ <: T →
        (∀ {S′ T′} → Overload G (S′ ⇒ T′) → T′ <: T → (S′ <: S₀)) →
        -----------------------
        LargestSrc G

    largest : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → LargestSrc G
    largest {S′ ⇒ T′} _ G⊆F with dec-tgt (G⊆F here)
    largest {S′ ⇒ T′} _ G⊆F | Left T′≮:T = defn Sᵇ Tᵇ oᵇ Tᵇ<:T (λ { here T′<:T → CONTRADICTION (<:-impl-¬≮: T′<:T T′≮:T) })
    largest {S′ ⇒ T′} _ G⊆F | Right T′<:T = defn S′ T′ (G⊆F here) T′<:T (λ { here _ → <:-refl })
    largest (Gᶠ ∩ Hᶠ) GH⊆F with largest Gᶠ (GH⊆F ∘ left) | largest Hᶠ (GH⊆F ∘ right)
    largest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ T₁<:T src₁ | defn S₂ T₂ o₂ T₂<:T src₂ with sat-∪ o₁ o₂
    largest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ T₁<:T src₁ | defn S₂ T₂ o₂ T₂<:T src₂ | defn o src tgt = defn _ _ o (<:-trans tgt (<:-∪-lub T₁<:T T₂<:T))
      (λ { (left o) T′<:T → <:-trans (src₁ o T′<:T) (<:-trans <:-∪-left src) ; (right o) T′<:T → <:-trans (src₂ o T′<:T) (<:-trans <:-∪-right src) })

    result₀ : LargestSrc F → Either (F ≮: (S ⇒ T)) (F <: (S ⇒ T))
    result₀ (defn S₀ T₀ o₀ T₀<:T src₀) with dec-src o₀
    result₀ (defn S₀ T₀ o₀ T₀<:T src₀) | Right S<:S₀ = Right (ov-<: o₀ (<:-function S<:S₀ T₀<:T))
    result₀ (defn S₀ T₀ o₀ T₀<:T src₀) | Left (witness s Ss ¬S₀s) = Left (result₁ (smallest Fᶠ (λ o → o))) where

      data SmallestTgt (G : Type) : Set where

        defn : ∀ S₁ T₁ →

          Overload F (S₁ ⇒ T₁) →
          Language S₁ s →
          (∀ {S′ T′} → Overload G (S′ ⇒ T′) → Language S′ s → (T₁ <: T′)) →
          -----------------------
          SmallestTgt G

      smallest : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → SmallestTgt G
      smallest {S′ ⇒ T′} _ G⊆F with dec-language S′ s
      smallest {S′ ⇒ T′} _ G⊆F | Left ¬S′s = defn Sᵗ Tᵗ oᵗ (S<:Sᵗ s Ss) λ { here S′s → CONTRADICTION (language-comp s ¬S′s S′s) }
      smallest {S′ ⇒ T′} _ G⊆F | Right S′s = defn S′ T′ (G⊆F here) S′s (λ { here _ → <:-refl })
      smallest (Gᶠ ∩ Hᶠ) GH⊆F with smallest Gᶠ (GH⊆F ∘ left) | smallest Hᶠ (GH⊆F ∘ right)
      smallest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ R₁s tgt₁ | defn S₂ T₂ o₂ R₂s tgt₂ with sat-∩ o₁ o₂
      smallest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ R₁s tgt₁ | defn S₂ T₂ o₂ R₂s tgt₂ | defn o src tgt = defn _ _ o (src s (R₁s , R₂s))
        (λ { (left o) S′s → <:-trans (<:-trans tgt <:-∩-left) (tgt₁ o S′s) ; (right o) S′s → <:-trans (<:-trans tgt <:-∩-right) (tgt₂ o S′s)} )

      result₁ : SmallestTgt F → (F ≮: (S ⇒ T))
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) with dec-tgt o₁
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) | Right T₁<:T = CONTRADICTION (language-comp s ¬S₀s (src₀ o₁ T₁<:T s S₁s))
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) | Left (witness t T₁t ¬Tt) = witness (function-ok s t) (ov-language Fᶠ lemma) (function-ok Ss ¬Tt) where

        lemma : ∀ {S′ T′} → Overload F (S′ ⇒ T′) → Language (S′ ⇒ T′) (function-ok s t)
        lemma {S′} o with dec-language S′ s
        lemma {S′} o | Left ¬S′s = function-ok₁ ¬S′s
        lemma {S′} o | Right S′s = function-ok₂ (tgt₁ o S′s t T₁t)
