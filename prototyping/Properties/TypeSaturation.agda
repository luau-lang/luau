{-# OPTIONS --rewriting #-}

module Properties.TypeSaturation where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right)
open import Luau.Subtyping using (Tree; Language; ¬Language; _<:_; _≮:_; witness; scalar; function; function-err; function-ok; function-ok₁; function-ok₂; scalar-function; _,_)
open import Luau.Type using (Type; _⇒_; _∩_; _∪_; never; unknown)
open import Luau.TypeNormalization using (_⇒ⁿ_; _∩ⁿ_; _∪ⁿ_)
open import Luau.TypeSaturation using (_⋓_; _⋒_; _∩ᵘ_; _∩ⁱ_; ∪-saturate; ∩-saturate; saturate)
open import Properties.Subtyping using (dec-language; language-comp; <:-impl-⊇; <:-refl; <:-trans; <:-trans-≮:; <:-impl-¬≮: ; <:-never; <:-unknown; <:-function; <:-union; <:-∪-symm; <:-∪-left; <:-∪-right; <:-∪-lub; <:-∪-assocl; <:-∪-assocr; <:-intersect; <:-∩-symm; <:-∩-left; <:-∩-right; <:-∩-glb; ≮:-function-left; ≮:-function-right; <:-function-never; <:-∩-assocl; <:-∩-assocr; ∩-<:-∪; <:-∩-distl-∪; ∩-distl-∪-<:; <:-∩-distr-∪; ∩-distr-∪-<:)
open import Properties.TypeNormalization using (FunType; function; _⇒_; _∩_; _∪_; never; unknown; inhabitant; inhabited; function-top; normal-⇒ⁿ; normal-∪ⁿ; normal-∩ⁿ; normalⁱ; <:-tgtⁿ; ∪ⁿ-<:-∪; ∪-<:-∪ⁿ; ∩ⁿ-<:-∩; ∩-<:-∩ⁿ)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.Functions using (_∘_)

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

-- Overloads F is the set of overloads of F (including never ⇒ never).
data Overloads : Type → Type → Set where

   never : ∀ {F} → Overloads F (never ⇒ never)
   here : ∀ {S T} → Overloads (S ⇒ T) (S ⇒ T)
   left : ∀ {S T F G} → Overloads F (S ⇒ T) → Overloads (F ∩ G) (S ⇒ T)
   right : ∀ {S T F G} → Overloads G (S ⇒ T) → Overloads (F ∩ G) (S ⇒ T)

-- An inductive presentation of the overloads of F ⋓ G
data ∪-Lift (P Q : Type → Set) : Type → Set where

  union : ∀ {R S T U} →

    P (R ⇒ S) →
    Q (T ⇒ U) →
    --------------------
    ∪-Lift P Q ((R ∪ T) ⇒ (S ∪ U))

-- An inductive presentation of the overloads of F ⋒ G
data ∩-Lift (P Q : Type → Set) : Type → Set where

  intersect : ∀ {R S T U} →

    P (R ⇒ S) →
    Q (T ⇒ U) →
    --------------------
    ∩-Lift P Q ((R ∩ T) ⇒ (S ∩ U))

-- An inductive presentation of the overloads of ∪-saturate F
data ∪-Saturate (P : Type → Set) : Type → Set where

  base : ∀ {S T} →

    P (S ⇒ T) →
    --------------------
    ∪-Saturate P (S ⇒ T)

  union : ∀ {R S T U} →

    ∪-Saturate P (R ⇒ S) →
    ∪-Saturate P (T ⇒ U) →
    --------------------
    ∪-Saturate P ((R ∪ T) ⇒ (S ∪ U))

-- An inductive presentation of the overloads of ∩-saturate F
data ∩-Saturate (P : Type → Set) : Type → Set where

  base : ∀ {S T} →

    P (S ⇒ T) →
    --------------------
    ∩-Saturate P (S ⇒ T)

  intersect : ∀ {R S T U} →

    ∩-Saturate P (R ⇒ S) →
    ∩-Saturate P (T ⇒ U) →
    --------------------
    ∩-Saturate P ((R ∩ T) ⇒ (S ∩ U))

-- The <:-up-closure of a set of function types
data <:-Close (P : Type → Set) : Type → Set where

  defn : ∀ {R S T U} →

    P (S ⇒ T) →
    R <: S →
    T <: U →
    ------------------
    <:-Close P (R ⇒ U)

-- F ⊆ᵒ G whenever every overload of F is an overload of G
_⊆ᵒ_ : Type → Type → Set
F ⊆ᵒ G = ∀ {S T} → Overloads F (S ⇒ T) → Overloads G (S ⇒ T)

-- P ⊂: Q when any type in P is a subtype of some type in Q
_⊂:_ : (Type → Set) → (Type → Set) → Set
P ⊂: Q = ∀ {S T} → P (S ⇒ T) → <:-Close Q (S ⇒ T)

-- <:-Close is a monad
just : ∀ {P S T} → P (S ⇒ T) → <:-Close P (S ⇒ T)
just p = {!!}

infixl 5 _>>=_ _>>=ˡ_ _>>=ʳ_
_>>=_ : ∀ {P Q S T} → <:-Close P (S ⇒ T) → (P ⊂: Q) → <:-Close Q (S ⇒ T)
p >>= P⊂Q = {!!}

_>>=ˡ_ : ∀ {P R S T} → <:-Close P (S ⇒ T) → (R <: S) → <:-Close P (R ⇒ T)
_>>=ˡ_ = {!!}

_>>=ʳ_ : ∀ {P S T U} → <:-Close P (S ⇒ T) → (T <: U) → <:-Close P (S ⇒ U)
_>>=ʳ_ = {!!}

-- F <:ᵒ (S ⇒ T) when (S ⇒ T) is a supertype of an overload of F
_<:ᵒ_ : Type → Type → Set
_<:ᵒ_ F = <:-Close (Overloads F)

⊂:-∩-saturate-inj : ∀ {P} → P ⊂: ∩-Saturate P
⊂:-∩-saturate-inj p = defn (base p) <:-refl <:-refl

⊂:-∩-saturate-indn : ∀ {P Q} → (P ⊂: Q) → (∩-Lift Q Q ⊂: Q) → (∩-Saturate P ⊂: Q)
⊂:-∩-saturate-indn = {!!}

⊂:-∩-saturate : ∀ {P Q} → (P ⊂: Q) → (∩-Saturate P ⊂: ∩-Saturate Q)
⊂:-∩-saturate P⊂Q p = {!!}

⊂:-∪-saturate-inj : ∀ {P} → P ⊂: ∪-Saturate P
⊂:-∪-saturate-inj p = defn (base p) (λ t z → z) (λ t z → z)

⊂:-∪-saturate : ∀ {P Q} → (P ⊂: Q) → (∪-Saturate P ⊂: ∪-Saturate Q)
⊂:-∪-saturate P⊂Q p = {!!}

_[∪]_ : ∀ {P Q R S T U} → <:-Close P (R ⇒ S) → <:-Close Q (T ⇒ U) → <:-Close (∪-Lift P Q) ((R ∪ T) ⇒ (S ∪ U))
p [∪] q = {!!}

_[∩]_ : ∀ {P Q R S T U} → <:-Close P (R ⇒ S) → <:-Close Q (T ⇒ U) → <:-Close (∩-Lift P Q) ((R ∩ T) ⇒ (S ∩ U))
p [∩] q = {!!}

⊂:-∩-saturate-lift : ∀ {P} → (∪-Lift P P ⊂: P) → (∪-Saturate P ⊂: P)
⊂:-∩-saturate-lift ∪P⊂P (base p) = defn p <:-refl <:-refl
⊂:-∩-saturate-lift ∪P⊂P (union p q) = (⊂:-∩-saturate-lift ∪P⊂P p [∪] ⊂:-∩-saturate-lift ∪P⊂P q) >>= ∪P⊂P

⊂:-∩-lift-saturate : ∀ {P} → ∩-Lift (∩-Saturate P) (∩-Saturate P) ⊂: ∩-Saturate P
⊂:-∩-lift-saturate (intersect p q) = defn (intersect p q) <:-refl <:-refl

⊂:-∪-lift-saturate : ∀ {P} → ∪-Lift (∪-Saturate P) (∪-Saturate P) ⊂: ∪-Saturate P
⊂:-∪-lift-saturate = {!!}

∪-saturate-resp-∩-saturation : ∀ {P} → (∩-Lift P P ⊂: P) → (∩-Lift (∪-Saturate P) (∪-Saturate P) ⊂: ∪-Saturate P)
∪-saturate-resp-∩-saturation ∩P⊂P (intersect (base p) (base q)) = ∩P⊂P (intersect p q) >>= ⊂:-∪-saturate-inj
∪-saturate-resp-∩-saturation ∩P⊂P (intersect p (union q q₁)) = (∪-saturate-resp-∩-saturation ∩P⊂P (intersect p q) [∪] ∪-saturate-resp-∩-saturation ∩P⊂P (intersect p q₁)) >>= ⊂:-∪-lift-saturate >>=ˡ <:-∩-distl-∪ >>=ʳ ∩-distl-∪-<:
∪-saturate-resp-∩-saturation ∩P⊂P (intersect (union p p₁) q) = (∪-saturate-resp-∩-saturation ∩P⊂P (intersect p q) [∪] ∪-saturate-resp-∩-saturation ∩P⊂P (intersect p₁ q)) >>= ⊂:-∪-lift-saturate >>=ˡ <:-∩-distr-∪ >>=ʳ ∩-distr-∪-<:

ov-language : ∀ {F t} → FunType F → (∀ {S T} → Overloads F (S ⇒ T) → Language (S ⇒ T) t) → Language F t
ov-language function p = p here
ov-language (S ⇒ T) p = p here
ov-language (F ∩ G) p = (ov-language F (p ∘ left) , ov-language G (p ∘ right))

ov-<: : ∀ {F R S T U} → FunType F → Overloads F (R ⇒ S) → ((R ⇒ S) <: (T ⇒ U)) → F <: (T ⇒ U)
ov-<: F never p = <:-trans (<:-trans (function-top {!!}) <:-function-never) p
ov-<: F here p = p
ov-<: (F ∩ G) (left o) p = <:-trans <:-∩-left (ov-<: F o p)
ov-<: (F ∩ G) (right o) p = <:-trans <:-∩-right (ov-<: G o p)

⊂:-overloads-left : ∀ {F G} → Overloads F ⊂: Overloads (F ∩ G)
⊂:-overloads-left = {!!}

⊂:-overloads-right : ∀ {F G} → Overloads G ⊂: Overloads (F ∩ G)
⊂:-overloads-right = {!!}

⊂:-overloads-⋒ : ∀ {F G} → FunType F → FunType G → ∩-Lift (Overloads F) (Overloads G) ⊂: Overloads (F ⋒ G)
⊂:-overloads-⋒ F G = {!!}

⊂:-⋒-overloads : ∀ {F G} → FunType F → FunType G → Overloads (F ⋒ G) ⊂: ∩-Lift (Overloads F) (Overloads G)
⊂:-⋒-overloads F G = {!!}

∪-saturate-overloads : ∀ {F} → FunType F → Overloads (∪-saturate F) ⊂: ∪-Saturate (Overloads F)
∪-saturate-overloads F = {!!}

overloads-∪-saturate : ∀ {F} → FunType F → ∪-Saturate (Overloads F) ⊂: Overloads (∪-saturate F)
overloads-∪-saturate F = {!!}

∪-saturated : ∀ {F} → FunType F → ∪-Lift (Overloads (∪-saturate F)) (Overloads (∪-saturate F)) ⊂: Overloads (∪-saturate F)
∪-saturated F = {!!}

∩-saturate-overloads : ∀ {F} → FunType F → Overloads (∩-saturate F) ⊂: ∩-Saturate (Overloads F)
∩-saturate-overloads F = {!!}

overloads-∩-saturate : ∀ {F} → FunType F → ∩-Saturate (Overloads F) ⊂: Overloads (∩-saturate F)
overloads-∩-saturate F = ⊂:-∩-saturate-indn (inj F) (step F) where
  
  inj :  ∀ {F} → FunType F → Overloads F ⊂: Overloads (∩-saturate F)
  inj = {!!}

  step : ∀ {F} → FunType F → ∩-Lift (Overloads (∩-saturate F)) (Overloads (∩-saturate F)) ⊂: Overloads (∩-saturate F)
  step function (intersect here here) = defn here <:-∩-left (<:-∩-glb <:-refl <:-refl)
  step (S ⇒ T) (intersect here here) = defn here <:-∩-left (<:-∩-glb <:-refl <:-refl)
  step (F ∩ G) (intersect (left (left p)) (left (left q))) = step F (intersect p q) >>= ⊂:-overloads-left >>= ⊂:-overloads-left
  step (F ∩ G) (intersect (left (left p)) (left (right q))) = ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) (intersect p q) >>= ⊂:-overloads-right
  step (F ∩ G) (intersect (left (right p)) (left (left q))) = ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) (intersect q p) >>= ⊂:-overloads-right >>=ˡ <:-∩-symm >>=ʳ <:-∩-symm
  step (F ∩ G) (intersect (left (right p)) (left (right q))) = step G (intersect p q) >>= ⊂:-overloads-right >>= ⊂:-overloads-left
  step (F ∩ G) (intersect (right p) q) with ⊂:-⋒-overloads (normal-∩-saturate F) (normal-∩-saturate G) p
  step (F ∩ G) (intersect (right p) (left (left q))) | defn (intersect p₁ p₂) p₃ p₄ =
    (step F (intersect p₁ q) [∩] just p₂) >>=
    ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-intersect p₃ <:-refl) (<:-∩-glb (<:-intersect <:-∩-left <:-refl) (<:-trans <:-∩-left <:-∩-right)) >>=ʳ
    <:-trans (<:-∩-glb (<:-intersect <:-∩-left <:-refl) (<:-trans <:-∩-left <:-∩-right)) (<:-intersect p₄ <:-refl)
  step (F ∩ G) (intersect (right p) (left (right q))) | defn (intersect p₁ p₂) p₃ p₄ =
    (just p₁ [∩] step G (intersect p₂ q)) >>=
    ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-intersect p₃ <:-refl) <:-∩-assocr >>=ʳ
    <:-trans <:-∩-assocl (<:-intersect p₄ <:-refl)
  step (F ∩ G) (intersect (right p) (right q)) | defn (intersect p₁ p₂) p₃ p₄ with ⊂:-⋒-overloads (normal-∩-saturate F) (normal-∩-saturate G) q
  step (F ∩ G) (intersect (right p) (right q)) | defn (intersect p₁ p₂) p₃ p₄ | defn (intersect q₁ q₂) q₃ q₄ =
    (step F (intersect p₁ q₁) [∩] step G (intersect p₂ q₂)) >>=
    ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-intersect p₃ q₃) (<:-∩-glb (<:-intersect <:-∩-left <:-∩-left) (<:-intersect <:-∩-right <:-∩-right)) >>=ʳ
    <:-trans (<:-∩-glb (<:-intersect <:-∩-left <:-∩-left) (<:-intersect <:-∩-right <:-∩-right)) (<:-intersect p₄ q₄)
  step (F ∩ G) (intersect (right p) (left never)) | defn (intersect p₁ p₂) p₃ p₄ = defn never <:-∩-right <:-never
  step (F ∩ G) (intersect (right p) never) | defn (intersect p₁ p₂) p₃ p₄ = defn never <:-∩-right <:-never
  step (F ∩ G) (intersect p (right q)) with ⊂:-⋒-overloads (normal-∩-saturate F) (normal-∩-saturate G) q
  step (F ∩ G) (intersect (left (left p)) (right q)) | defn (intersect q₁ q₂) q₃ q₄ =
    (step F (intersect p q₁) [∩] just q₂) >>=
    ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-intersect <:-refl q₃) <:-∩-assocl >>=ʳ
    <:-trans <:-∩-assocr (<:-intersect <:-refl q₄)
  step (F ∩ G) (intersect (left (right p)) (right q)) | defn (intersect q₁ q₂) q₃ q₄ =
    (just q₁ [∩] step G (intersect p q₂) ) >>=
    ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-intersect <:-refl q₃) (<:-∩-glb (<:-trans <:-∩-right <:-∩-left) (<:-∩-glb <:-∩-left (<:-trans <:-∩-right <:-∩-right))) >>=ʳ
    <:-∩-glb (<:-trans <:-∩-right <:-∩-left) (<:-trans (<:-∩-glb <:-∩-left (<:-trans <:-∩-right <:-∩-right)) q₄)
  step (F ∩ G) (intersect (right p) (right q)) | defn (intersect q₁ q₂) q₃ q₄ with ⊂:-⋒-overloads (normal-∩-saturate F) (normal-∩-saturate G) p
  step (F ∩ G) (intersect (right p) (right q)) | defn (intersect q₁ q₂) q₃ q₄ | defn (intersect p₁ p₂) p₃ p₄ =
    (step F (intersect p₁ q₁) [∩] step G (intersect p₂ q₂)) >>=
    ⊂:-overloads-⋒ (normal-∩-saturate F) (normal-∩-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-intersect p₃ q₃) (<:-∩-glb (<:-intersect <:-∩-left <:-∩-left) (<:-intersect <:-∩-right <:-∩-right)) >>=ʳ
    <:-trans (<:-∩-glb (<:-intersect <:-∩-left <:-∩-left) (<:-intersect <:-∩-right <:-∩-right)) (<:-intersect p₄ q₄)
  step (F ∩ G) (intersect never (right q)) | defn (intersect q₁ q₂) q₃ q₄ = defn never <:-∩-left <:-never
  step (F ∩ G) (intersect (left never) (right q)) | defn (intersect q₁ q₂) q₃ q₄ = defn never <:-∩-left <:-never
  step (F ∩ G) (intersect (left never) q) = defn never <:-∩-left <:-never
  step (F ∩ G) (intersect p (left never)) = defn never <:-∩-right <:-never
  step F (intersect never q) = defn never <:-∩-left <:-never
  step F (intersect p never) = defn never <:-∩-right <:-never

saturate-overloads : ∀ {F} → FunType F → Overloads (saturate F) ⊂: ∪-Saturate (∩-Saturate (Overloads F))
saturate-overloads F o = ∪-saturate-overloads (normal-∩-saturate F) o >>= (⊂:-∪-saturate (∩-saturate-overloads F))

overloads-saturate : ∀ {F} → FunType F → ∪-Saturate (∩-Saturate (Overloads F)) ⊂: Overloads (saturate F)
overloads-saturate F o = ⊂:-∪-saturate (overloads-∩-saturate F) o >>= overloads-∪-saturate (normal-∩-saturate F)

-- Saturated F whenever
-- * if F has overloads (R ⇒ S) and (T ⇒ U) then F has an overload which is a subtype of ((R ∩ T) ⇒ (S ∩ U))
-- * ditto union
data Saturated (F : Type) : Set where

  defn : 

    (∀ {R S T U} → Overloads F (R ⇒ S) → Overloads F (T ⇒ U) → F <:ᵒ ((R ∩ T) ⇒ (S ∩ U))) →
    (∀ {R S T U} → Overloads F (R ⇒ S) → Overloads F (T ⇒ U) → F <:ᵒ ((R ∪ T) ⇒ (S ∪ U))) →
    -----------
    Saturated F

-- saturated F is saturated!
saturated : ∀ {F} → FunType F → Saturated (saturate F)
saturated F = defn
  (λ n o → (saturate-overloads F n [∩] saturate-overloads F o) >>= ∪-saturate-resp-∩-saturation ⊂:-∩-lift-saturate >>= overloads-saturate F)
  (λ n o → ∪-saturated (normal-∩-saturate F) (union n o))

-- Subtyping is decidable on saturated normalized types

dec-<:-overloads : ∀ {F S T} → FunType F → FunType (S ⇒ T) → Saturated F →
  (∀ {S′ T′} → (Overloads F (S′ ⇒ T′)) → Either (S ≮: S′) (S <: S′)) →
  (∀ {S′ T′} → (Overloads F (S′ ⇒ T′)) → Either (T′ ≮: T) (T′ <: T)) →
  Either (F ≮: (S ⇒ T)) (F <: (S ⇒ T))
dec-<:-overloads {F} {S} {T} Fᶠ function _ _ _ = Right (function-top Fᶠ)
dec-<:-overloads {F} {S} {T} Fᶠ (Sⁱ ⇒ Tⁿ) (defn sat-∩ sat-∪) dec-src dec-tgt = result (top Fᶠ (λ o → o)) where

  data Top G : Set where

    defn : ∀ Sᵗ Tᵗ →

      Overloads F (Sᵗ ⇒ Tᵗ) →
      (∀ {S′ T′} → Overloads G (S′ ⇒ T′) → (S′ <: Sᵗ)) →
      -------------
      Top G

  top : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → Top G
  top {S′ ⇒ T′} _ G⊆F = defn S′ T′ (G⊆F here) (λ { here → <:-refl ; never → <:-never })
  top (Gᶠ ∩ Hᶠ) G⊆F with top Gᶠ (G⊆F ∘ left) | top Hᶠ (G⊆F ∘ right)
  top (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵗ Sᵗ p p₁ | defn Tᵗ Uᵗ q q₁ with sat-∪ p q
  top (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵗ Sᵗ p p₁ | defn Tᵗ Uᵗ q q₁ | defn n r r₁ = defn _ _ n
    (λ { (left o) → <:-trans (<:-trans (p₁ o) <:-∪-left) r ; (right o) → <:-trans (<:-trans (q₁ o) <:-∪-right) r ; never → <:-never })

  result : Top F → Either (F ≮: (S ⇒ T)) (F <: (S ⇒ T))
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) with dec-src oᵗ
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) | Left (witness s Ss ¬Sᵗs) = Left (witness (function-err s) (ov-language Fᶠ (λ o → function-err (<:-impl-⊇ (srcᵗ o) s ¬Sᵗs))) (function-err Ss))
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) | Right S<:Sᵗ = result₀ (largest Fᶠ (λ o → o)) where

    data LargestSrc (G : Type) : Set where

      defn : ∀ S₀ T₀ →

        Overloads F (S₀ ⇒ T₀) →
        T₀ <: T →
        (∀ {S′ T′} → Overloads G (S′ ⇒ T′) → T′ <: T → (S′ <: S₀)) →
        -----------------------
        LargestSrc G

    largest : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → LargestSrc G
    largest {S′ ⇒ T′} _ G⊆F with dec-tgt (G⊆F here)
    largest {S′ ⇒ T′} _ G⊆F | Left T′≮:T = defn never never never <:-never (λ { here T′<:T → CONTRADICTION (<:-impl-¬≮: T′<:T T′≮:T) ; never _ → <:-never })
    largest {S′ ⇒ T′} _ G⊆F | Right T′<:T = defn S′ T′ (G⊆F here) T′<:T (λ { here _ → <:-refl  ; never _ → <:-never })
    largest (Gᶠ ∩ Hᶠ) GH⊆F with largest Gᶠ (GH⊆F ∘ left) | largest Hᶠ (GH⊆F ∘ right)
    largest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ T₁<:T src₁ | defn S₂ T₂ o₂ T₂<:T src₂ with sat-∪ o₁ o₂
    largest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ T₁<:T src₁ | defn S₂ T₂ o₂ T₂<:T src₂ | defn o src tgt = defn _ _ o (<:-trans tgt (<:-∪-lub T₁<:T T₂<:T))
      (λ { (left o) T′<:T → <:-trans (src₁ o T′<:T) (<:-trans <:-∪-left src)
         ; (right o) T′<:T → <:-trans (src₂ o T′<:T) (<:-trans <:-∪-right src)
         ; never _ → <:-never })

    result₀ : LargestSrc F → Either (F ≮: (S ⇒ T)) (F <: (S ⇒ T))
    result₀ (defn S₀ T₀ o₀ T₀<:T src₀) with dec-src o₀
    result₀ (defn S₀ T₀ o₀ T₀<:T src₀) | Right S<:S₀ = Right (ov-<: Fᶠ o₀ (<:-function S<:S₀ T₀<:T))
    result₀ (defn S₀ T₀ o₀ T₀<:T src₀) | Left (witness s Ss ¬S₀s) = Left (result₁ (smallest Fᶠ (λ o → o))) where

      data SmallestTgt (G : Type) : Set where

        defn : ∀ S₁ T₁ →

          Overloads F (S₁ ⇒ T₁) →
          Language S₁ s →
          (∀ {S′ T′} → Overloads G (S′ ⇒ T′) → Language S′ s → (T₁ <: T′)) →
          -----------------------
          SmallestTgt G

      smallest : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → SmallestTgt G
      smallest {S′ ⇒ T′} _ G⊆F with dec-language S′ s
      smallest {S′ ⇒ T′} _ G⊆F | Left ¬S′s = defn Sᵗ Tᵗ oᵗ (S<:Sᵗ s Ss) λ { here S′s → CONTRADICTION (language-comp s ¬S′s S′s) ; never (scalar ()) }
      smallest {S′ ⇒ T′} _ G⊆F | Right S′s = defn S′ T′ (G⊆F here) S′s (λ { here _ → <:-refl ; never (scalar ()) })
      smallest (Gᶠ ∩ Hᶠ) GH⊆F with smallest Gᶠ (GH⊆F ∘ left) | smallest Hᶠ (GH⊆F ∘ right)
      smallest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ R₁s tgt₁ | defn S₂ T₂ o₂ R₂s tgt₂ with sat-∩ o₁ o₂
      smallest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ R₁s tgt₁ | defn S₂ T₂ o₂ R₂s tgt₂ | defn o src tgt = defn _ _ o (src s (R₁s , R₂s))
        (λ { (left o) S′s → <:-trans (<:-trans tgt <:-∩-left) (tgt₁ o S′s)
           ; (right o) S′s → <:-trans (<:-trans tgt <:-∩-right) (tgt₂ o S′s)
           ; never (scalar ()) } )

      result₁ : SmallestTgt F → (F ≮: (S ⇒ T))
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) with dec-tgt o₁
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) | Right T₁<:T = CONTRADICTION (language-comp s ¬S₀s (src₀ o₁ T₁<:T s S₁s))
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) | Left (witness t T₁t ¬Tt) = witness (function-ok s t) (ov-language Fᶠ lemma) (function-ok Ss ¬Tt) where

        lemma : ∀ {S′ T′} → Overloads F (S′ ⇒ T′) → Language (S′ ⇒ T′) (function-ok s t)
        lemma {S′} o with dec-language S′ s
        lemma {S′} o | Left ¬S′s = function-ok₁ ¬S′s
        lemma {S′} o | Right S′s = function-ok₂ (tgt₁ o S′s t T₁t)
