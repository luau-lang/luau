{-# OPTIONS --rewriting #-}

module Properties.TypeSaturation where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right)
open import Luau.Subtyping using (Tree; Language; ¬Language; _<:_; _≮:_; witness; scalar; function; function-err; function-ok; function-ok₁; function-ok₂; scalar-function; _,_; never)
open import Luau.Type using (Type; _⇒_; _∩_; _∪_; never; unknown)
open import Luau.TypeNormalization using (_∩ⁿ_; _∪ⁿ_)
open import Luau.TypeSaturation using (_⋓_; _⋒_; _∩ᵘ_; _∩ⁱ_; ∪-saturate; ∩-saturate; saturate)
open import Properties.Subtyping using (dec-language; language-comp; <:-impl-⊇; <:-refl; <:-trans; <:-trans-≮:; <:-impl-¬≮: ; <:-never; <:-unknown; <:-function; <:-union; <:-∪-symm; <:-∪-left; <:-∪-right; <:-∪-lub; <:-∪-assocl; <:-∪-assocr; <:-intersect; <:-∩-symm; <:-∩-left; <:-∩-right; <:-∩-glb; ≮:-function-left; ≮:-function-right; <:-function-∩-∪; <:-function-∩-∩; <:-∩-assocl; <:-∩-assocr; ∩-<:-∪; <:-∩-distl-∪; ∩-distl-∪-<:; <:-∩-distr-∪; ∩-distr-∪-<:)
open import Properties.TypeNormalization using (Normal; FunType; _⇒_; _∩_; _∪_; never; unknown; normal-∪ⁿ; normal-∩ⁿ; ∪ⁿ-<:-∪; ∪-<:-∪ⁿ; ∩ⁿ-<:-∩; ∩-<:-∩ⁿ)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.Functions using (_∘_)

-- Saturation preserves normalization
normal-⋒ : ∀ {F G} → FunType F → FunType G → FunType (F ⋒ G)
normal-⋒ (R ⇒ S) (T ⇒ U) = (normal-∩ⁿ R T) ⇒ (normal-∩ⁿ S U)
normal-⋒ (R ⇒ S) (G ∩ H) = normal-⋒ (R ⇒ S) G ∩ normal-⋒ (R ⇒ S) H
normal-⋒ (E ∩ F) G = normal-⋒ E G ∩ normal-⋒ F G

normal-⋓ : ∀ {F G} → FunType F → FunType G → FunType (F ⋓ G)
normal-⋓ (R ⇒ S) (T ⇒ U) = (normal-∪ⁿ R T) ⇒ (normal-∪ⁿ S U)
normal-⋓ (R ⇒ S) (G ∩ H) = normal-⋓ (R ⇒ S) G ∩ normal-⋓ (R ⇒ S) H
normal-⋓ (E ∩ F) G = normal-⋓ E G ∩ normal-⋓ F G

normal-∩-saturate : ∀ {F} → FunType F → FunType (∩-saturate F)
normal-∩-saturate (S ⇒ T) = S ⇒ T
normal-∩-saturate (F ∩ G) = (normal-∩-saturate F ∩ normal-∩-saturate G) ∩ normal-⋒ (normal-∩-saturate F) (normal-∩-saturate G)

normal-∪-saturate : ∀ {F} → FunType F → FunType (∪-saturate F)
normal-∪-saturate (S ⇒ T) = S ⇒ T
normal-∪-saturate (F ∩ G) = (normal-∪-saturate F ∩ normal-∪-saturate G) ∩ normal-⋓ (normal-∪-saturate F) (normal-∪-saturate G)

normal-saturate : ∀ {F} → FunType F → FunType (saturate F)
normal-saturate F = normal-∪-saturate (normal-∩-saturate F)

-- Saturation resects subtyping
∪-saturate-<: : ∀ {F} → FunType F → ∪-saturate F <: F
∪-saturate-<: (S ⇒ T) = <:-refl
∪-saturate-<: (F ∩ G) = <:-trans <:-∩-left (<:-intersect (∪-saturate-<: F) (∪-saturate-<: G))

∩-saturate-<: : ∀ {F} → FunType F → ∩-saturate F <: F
∩-saturate-<: (S ⇒ T) = <:-refl
∩-saturate-<: (F ∩ G) = <:-trans <:-∩-left (<:-intersect (∩-saturate-<: F) (∩-saturate-<: G))

saturate-<: : ∀ {F} → FunType F → saturate F <: F
saturate-<: F = <:-trans (∪-saturate-<: (normal-∩-saturate F)) (∩-saturate-<: F)

∩-<:-⋓ : ∀ {F G} → FunType F → FunType G → (F ∩ G) <: (F ⋓ G)
∩-<:-⋓ (R ⇒ S) (T ⇒ U) = <:-trans <:-function-∩-∪ (<:-function (∪ⁿ-<:-∪ R T) (∪-<:-∪ⁿ S U))
∩-<:-⋓ (R ⇒ S) (G ∩ H) = <:-trans (<:-∩-glb (<:-intersect <:-refl <:-∩-left) (<:-intersect <:-refl <:-∩-right)) (<:-intersect (∩-<:-⋓ (R ⇒ S) G) (∩-<:-⋓ (R ⇒ S) H))
∩-<:-⋓ (E ∩ F) G = <:-trans (<:-∩-glb (<:-intersect <:-∩-left <:-refl) (<:-intersect <:-∩-right <:-refl)) (<:-intersect (∩-<:-⋓ E G) (∩-<:-⋓ F G))

∩-<:-⋒ : ∀ {F G} → FunType F → FunType G → (F ∩ G) <: (F ⋒ G)
∩-<:-⋒ (R ⇒ S) (T ⇒ U) = <:-trans <:-function-∩-∩ (<:-function (∩ⁿ-<:-∩ R T) (∩-<:-∩ⁿ S U))
∩-<:-⋒ (R ⇒ S) (G ∩ H) = <:-trans (<:-∩-glb (<:-intersect <:-refl <:-∩-left) (<:-intersect <:-refl <:-∩-right)) (<:-intersect (∩-<:-⋒ (R ⇒ S) G) (∩-<:-⋒ (R ⇒ S) H))
∩-<:-⋒ (E ∩ F) G = <:-trans (<:-∩-glb (<:-intersect <:-∩-left <:-refl) (<:-intersect <:-∩-right <:-refl)) (<:-intersect (∩-<:-⋒ E G) (∩-<:-⋒ F G))

<:-∪-saturate : ∀ {F} → FunType F → F <: ∪-saturate F
<:-∪-saturate (S ⇒ T) = <:-refl
<:-∪-saturate (F ∩ G) = <:-∩-glb (<:-intersect (<:-∪-saturate F) (<:-∪-saturate G)) (<:-trans (<:-intersect (<:-∪-saturate F) (<:-∪-saturate G)) (∩-<:-⋓ (normal-∪-saturate F) (normal-∪-saturate G)))

<:-∩-saturate : ∀ {F} → FunType F → F <: ∩-saturate F
<:-∩-saturate (S ⇒ T) = <:-refl
<:-∩-saturate (F ∩ G) = <:-∩-glb (<:-intersect (<:-∩-saturate F) (<:-∩-saturate G)) (<:-trans (<:-intersect (<:-∩-saturate F) (<:-∩-saturate G)) (∩-<:-⋒ (normal-∩-saturate F) (normal-∩-saturate G)))

<:-saturate : ∀ {F} → FunType F → F <: saturate F
<:-saturate F = <:-trans (<:-∩-saturate F) (<:-∪-saturate (normal-∩-saturate F))

-- Overloads F is the set of overloads of F
data Overloads : Type → Type → Set where

   here : ∀ {S T} → Overloads (S ⇒ T) (S ⇒ T)
   left : ∀ {S T F G} → Overloads F (S ⇒ T) → Overloads (F ∩ G) (S ⇒ T)
   right : ∀ {S T F G} → Overloads G (S ⇒ T) → Overloads (F ∩ G) (S ⇒ T)

normal-overload-src : ∀ {F S T} → FunType F → Overloads F (S ⇒ T) → Normal S
normal-overload-src (S ⇒ T) here = S
normal-overload-src (F ∩ G) (left o) = normal-overload-src F o
normal-overload-src (F ∩ G) (right o) = normal-overload-src G o

normal-overload-tgt : ∀ {F S T} → FunType F → Overloads F (S ⇒ T) → Normal T
normal-overload-tgt (S ⇒ T) here = T
normal-overload-tgt (F ∩ G) (left o) = normal-overload-tgt F o
normal-overload-tgt (F ∩ G) (right o) = normal-overload-tgt G o

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

-- F <:ᵒ G when every overload of G is a supertype of an overload of F
_<:ᵒ_ : Type → Type → Set
_<:ᵒ_ F G = ∀ {S T} → Overloads G (S ⇒ T) → <:-Close (Overloads F) (S ⇒ T)

-- P ⊂: Q when any type in P is a subtype of some type in Q
_⊂:_ : (Type → Set) → (Type → Set) → Set
P ⊂: Q = ∀ {S T} → P (S ⇒ T) → <:-Close Q (S ⇒ T)

-- <:-Close is a monad
just : ∀ {P S T} → P (S ⇒ T) → <:-Close P (S ⇒ T)
just p = defn p <:-refl <:-refl

infixl 5 _>>=_ _>>=ˡ_ _>>=ʳ_
_>>=_ : ∀ {P Q S T} → <:-Close P (S ⇒ T) → (P ⊂: Q) → <:-Close Q (S ⇒ T)
(defn p p₁ p₂) >>= P⊂Q with P⊂Q p
(defn p p₁ p₂) >>= P⊂Q | defn q q₁ q₂ = defn q (<:-trans p₁ q₁) (<:-trans q₂ p₂)

_>>=ˡ_ : ∀ {P R S T} → <:-Close P (S ⇒ T) → (R <: S) → <:-Close P (R ⇒ T)
(defn p p₁ p₂) >>=ˡ q = defn p (<:-trans q p₁) p₂

_>>=ʳ_ : ∀ {P S T U} → <:-Close P (S ⇒ T) → (T <: U) → <:-Close P (S ⇒ U)
(defn p p₁ p₂) >>=ʳ q = defn p p₁ (<:-trans p₂ q)

-- Properties of ⊂:
⊂:-refl : ∀ {P} → P ⊂: P
⊂:-refl p = just p

_[∪]_ : ∀ {P Q R S T U} → <:-Close P (R ⇒ S) → <:-Close Q (T ⇒ U) → <:-Close (∪-Lift P Q) ((R ∪ T) ⇒ (S ∪ U))
(defn p p₁ p₂) [∪] (defn q q₁ q₂) = defn (union p q) (<:-union p₁ q₁) (<:-union p₂ q₂)

_[∩]_ : ∀ {P Q R S T U} → <:-Close P (R ⇒ S) → <:-Close Q (T ⇒ U) → <:-Close (∩-Lift P Q) ((R ∩ T) ⇒ (S ∩ U))
(defn p p₁ p₂) [∩] (defn q q₁ q₂) = defn (intersect p q) (<:-intersect p₁ q₁) (<:-intersect p₂ q₂)

⊂:-∩-saturate-inj : ∀ {P} → P ⊂: ∩-Saturate P
⊂:-∩-saturate-inj p = defn (base p) <:-refl <:-refl

⊂:-∪-saturate-inj : ∀ {P} → P ⊂: ∪-Saturate P
⊂:-∪-saturate-inj p = just (base p)

⊂:-∩-lift-saturate : ∀ {P} → ∩-Lift (∩-Saturate P) (∩-Saturate P) ⊂: ∩-Saturate P
⊂:-∩-lift-saturate (intersect p q) = just (intersect p q)

⊂:-∪-lift-saturate : ∀ {P} → ∪-Lift (∪-Saturate P) (∪-Saturate P) ⊂: ∪-Saturate P
⊂:-∪-lift-saturate (union p q) = just (union p q)

⊂:-∩-lift : ∀ {P Q R S} → (P ⊂: Q) → (R ⊂: S) → (∩-Lift P R ⊂: ∩-Lift Q S)
⊂:-∩-lift P⊂Q R⊂S (intersect n o) = P⊂Q n [∩] R⊂S o

⊂:-∪-lift : ∀ {P Q R S} → (P ⊂: Q) → (R ⊂: S) → (∪-Lift P R ⊂: ∪-Lift Q S)
⊂:-∪-lift P⊂Q R⊂S (union n o) = P⊂Q n [∪] R⊂S o

⊂:-∩-saturate : ∀ {P Q} → (P ⊂: Q) → (∩-Saturate P ⊂: ∩-Saturate Q)
⊂:-∩-saturate P⊂Q (base p) = P⊂Q p >>= ⊂:-∩-saturate-inj
⊂:-∩-saturate P⊂Q (intersect p q) = (⊂:-∩-saturate P⊂Q p [∩] ⊂:-∩-saturate P⊂Q q) >>= ⊂:-∩-lift-saturate

⊂:-∪-saturate : ∀ {P Q} → (P ⊂: Q) → (∪-Saturate P ⊂: ∪-Saturate Q)
⊂:-∪-saturate P⊂Q (base p) = P⊂Q p >>= ⊂:-∪-saturate-inj
⊂:-∪-saturate P⊂Q (union p q) = (⊂:-∪-saturate P⊂Q p [∪] ⊂:-∪-saturate P⊂Q q) >>= ⊂:-∪-lift-saturate

⊂:-∩-saturate-indn : ∀ {P Q} → (P ⊂: Q) → (∩-Lift Q Q ⊂: Q) → (∩-Saturate P ⊂: Q)
⊂:-∩-saturate-indn P⊂Q QQ⊂Q (base p) = P⊂Q p
⊂:-∩-saturate-indn P⊂Q QQ⊂Q (intersect p q) = (⊂:-∩-saturate-indn P⊂Q QQ⊂Q p [∩] ⊂:-∩-saturate-indn P⊂Q QQ⊂Q q) >>= QQ⊂Q

⊂:-∪-saturate-indn : ∀ {P Q} → (P ⊂: Q) → (∪-Lift Q Q ⊂: Q) → (∪-Saturate P ⊂: Q)
⊂:-∪-saturate-indn P⊂Q QQ⊂Q (base p) = P⊂Q p
⊂:-∪-saturate-indn P⊂Q QQ⊂Q (union p q) = (⊂:-∪-saturate-indn P⊂Q QQ⊂Q p [∪] ⊂:-∪-saturate-indn P⊂Q QQ⊂Q q) >>= QQ⊂Q

∪-saturate-resp-∩-saturation : ∀ {P} → (∩-Lift P P ⊂: P) → (∩-Lift (∪-Saturate P) (∪-Saturate P) ⊂: ∪-Saturate P)
∪-saturate-resp-∩-saturation ∩P⊂P (intersect (base p) (base q)) = ∩P⊂P (intersect p q) >>= ⊂:-∪-saturate-inj
∪-saturate-resp-∩-saturation ∩P⊂P (intersect p (union q q₁)) = (∪-saturate-resp-∩-saturation ∩P⊂P (intersect p q) [∪] ∪-saturate-resp-∩-saturation ∩P⊂P (intersect p q₁)) >>= ⊂:-∪-lift-saturate >>=ˡ <:-∩-distl-∪ >>=ʳ ∩-distl-∪-<:
∪-saturate-resp-∩-saturation ∩P⊂P (intersect (union p p₁) q) = (∪-saturate-resp-∩-saturation ∩P⊂P (intersect p q) [∪] ∪-saturate-resp-∩-saturation ∩P⊂P (intersect p₁ q)) >>= ⊂:-∪-lift-saturate >>=ˡ <:-∩-distr-∪ >>=ʳ ∩-distr-∪-<:

ov-language : ∀ {F t} → FunType F → (∀ {S T} → Overloads F (S ⇒ T) → Language (S ⇒ T) t) → Language F t
ov-language (S ⇒ T) p = p here
ov-language (F ∩ G) p = (ov-language F (p ∘ left) , ov-language G (p ∘ right))

ov-<: : ∀ {F R S T U} → FunType F → Overloads F (R ⇒ S) → ((R ⇒ S) <: (T ⇒ U)) → F <: (T ⇒ U)
ov-<: F here p = p
ov-<: (F ∩ G) (left o) p = <:-trans <:-∩-left (ov-<: F o p)
ov-<: (F ∩ G) (right o) p = <:-trans <:-∩-right (ov-<: G o p)

<:ᵒ-impl-<: : ∀ {F G} → FunType F → FunType G → (F <:ᵒ G) → (F <: G)
<:ᵒ-impl-<: F (T ⇒ U) F<G with F<G here
<:ᵒ-impl-<: F (T ⇒ U) F<G | defn o o₁ o₂ = ov-<: F o (<:-function o₁ o₂)
<:ᵒ-impl-<: F (G ∩ H) F<G = <:-∩-glb (<:ᵒ-impl-<: F G (F<G ∘ left)) (<:ᵒ-impl-<: F H (F<G ∘ right))

⊂:-overloads-left : ∀ {F G} → Overloads F ⊂: Overloads (F ∩ G)
⊂:-overloads-left p = just (left p)

⊂:-overloads-right : ∀ {F G} → Overloads G ⊂: Overloads (F ∩ G)
⊂:-overloads-right p = just (right p)

⊂:-overloads-⋒ : ∀ {F G} → FunType F → FunType G → ∩-Lift (Overloads F) (Overloads G) ⊂: Overloads (F ⋒ G)
⊂:-overloads-⋒ (R ⇒ S) (T ⇒ U) (intersect here here) = defn here (∩-<:-∩ⁿ R T) (∩ⁿ-<:-∩ S U)
⊂:-overloads-⋒ (R ⇒ S) (G ∩ H) (intersect here (left o)) = ⊂:-overloads-⋒ (R ⇒ S) G (intersect here o) >>= ⊂:-overloads-left
⊂:-overloads-⋒ (R ⇒ S) (G ∩ H) (intersect here (right o)) = ⊂:-overloads-⋒ (R ⇒ S) H (intersect here o) >>= ⊂:-overloads-right
⊂:-overloads-⋒ (E ∩ F) G (intersect (left n) o) = ⊂:-overloads-⋒ E G (intersect n o) >>= ⊂:-overloads-left
⊂:-overloads-⋒ (E ∩ F) G (intersect (right n) o) = ⊂:-overloads-⋒ F G (intersect n o) >>= ⊂:-overloads-right

⊂:-⋒-overloads : ∀ {F G} → FunType F → FunType G → Overloads (F ⋒ G) ⊂: ∩-Lift (Overloads F) (Overloads G)
⊂:-⋒-overloads (R ⇒ S) (T ⇒ U) here = defn (intersect here here) (∩ⁿ-<:-∩ R T) (∩-<:-∩ⁿ S U)
⊂:-⋒-overloads (R ⇒ S) (G ∩ H) (left o) = ⊂:-⋒-overloads (R ⇒ S) G o >>= ⊂:-∩-lift ⊂:-refl ⊂:-overloads-left 
⊂:-⋒-overloads (R ⇒ S) (G ∩ H) (right o) = ⊂:-⋒-overloads (R ⇒ S) H o >>= ⊂:-∩-lift ⊂:-refl ⊂:-overloads-right
⊂:-⋒-overloads (E ∩ F) G (left o) = ⊂:-⋒-overloads E G o >>= ⊂:-∩-lift ⊂:-overloads-left ⊂:-refl
⊂:-⋒-overloads (E ∩ F) G (right o) = ⊂:-⋒-overloads F G o >>= ⊂:-∩-lift ⊂:-overloads-right ⊂:-refl

⊂:-overloads-⋓ : ∀ {F G} → FunType F → FunType G → ∪-Lift (Overloads F) (Overloads G) ⊂: Overloads (F ⋓ G)
⊂:-overloads-⋓ (R ⇒ S) (T ⇒ U) (union here here) = defn here (∪-<:-∪ⁿ R T) (∪ⁿ-<:-∪ S U)
⊂:-overloads-⋓ (R ⇒ S) (G ∩ H) (union here (left o)) = ⊂:-overloads-⋓ (R ⇒ S) G (union here o) >>= ⊂:-overloads-left
⊂:-overloads-⋓ (R ⇒ S) (G ∩ H) (union here (right o)) = ⊂:-overloads-⋓ (R ⇒ S) H (union here o) >>= ⊂:-overloads-right
⊂:-overloads-⋓ (E ∩ F) G (union (left n) o) = ⊂:-overloads-⋓ E G (union n o) >>= ⊂:-overloads-left
⊂:-overloads-⋓ (E ∩ F) G (union (right n) o) = ⊂:-overloads-⋓ F G (union n o) >>= ⊂:-overloads-right

⊂:-⋓-overloads : ∀ {F G} → FunType F → FunType G → Overloads (F ⋓ G) ⊂: ∪-Lift (Overloads F) (Overloads G)
⊂:-⋓-overloads (R ⇒ S) (T ⇒ U) here = defn (union here here) (∪ⁿ-<:-∪ R T) (∪-<:-∪ⁿ S U)
⊂:-⋓-overloads (R ⇒ S) (G ∩ H) (left o) = ⊂:-⋓-overloads (R ⇒ S) G o >>= ⊂:-∪-lift ⊂:-refl ⊂:-overloads-left
⊂:-⋓-overloads (R ⇒ S) (G ∩ H) (right o) = ⊂:-⋓-overloads (R ⇒ S) H o >>= ⊂:-∪-lift ⊂:-refl ⊂:-overloads-right
⊂:-⋓-overloads (E ∩ F) G (left o) = ⊂:-⋓-overloads E G o >>= ⊂:-∪-lift ⊂:-overloads-left ⊂:-refl
⊂:-⋓-overloads (E ∩ F) G (right o) = ⊂:-⋓-overloads F G o >>= ⊂:-∪-lift ⊂:-overloads-right ⊂:-refl

∪-saturate-overloads : ∀ {F} → FunType F → Overloads (∪-saturate F) ⊂: ∪-Saturate (Overloads F)
∪-saturate-overloads (S ⇒ T) here = just (base here)
∪-saturate-overloads (F ∩ G) (left (left o)) = ∪-saturate-overloads F o >>= ⊂:-∪-saturate ⊂:-overloads-left
∪-saturate-overloads (F ∩ G) (left (right o)) = ∪-saturate-overloads G o >>= ⊂:-∪-saturate ⊂:-overloads-right
∪-saturate-overloads (F ∩ G) (right o) =
  ⊂:-⋓-overloads (normal-∪-saturate F) (normal-∪-saturate G) o >>=
  ⊂:-∪-lift (∪-saturate-overloads F) (∪-saturate-overloads G) >>=
  ⊂:-∪-lift (⊂:-∪-saturate ⊂:-overloads-left) (⊂:-∪-saturate ⊂:-overloads-right) >>=
  ⊂:-∪-lift-saturate

overloads-∪-saturate : ∀ {F} → FunType F → ∪-Saturate (Overloads F) ⊂: Overloads (∪-saturate F)
overloads-∪-saturate F = ⊂:-∪-saturate-indn (inj F) (step F) where

  inj : ∀ {F} → FunType F → Overloads F ⊂: Overloads (∪-saturate F)
  inj (S ⇒ T) here = just here
  inj (F ∩ G) (left p) = inj F p >>= ⊂:-overloads-left >>= ⊂:-overloads-left
  inj (F ∩ G) (right p) = inj G p >>= ⊂:-overloads-right >>= ⊂:-overloads-left

  step : ∀ {F} → FunType F → ∪-Lift (Overloads (∪-saturate F)) (Overloads (∪-saturate F)) ⊂: Overloads (∪-saturate F)
  step (S ⇒ T) (union here here) = defn here (<:-∪-lub <:-refl <:-refl) <:-∪-left
  step (F ∩ G) (union (left (left p)) (left (left q))) = step F (union p q) >>= ⊂:-overloads-left >>= ⊂:-overloads-left
  step (F ∩ G) (union (left (left p)) (left (right q))) = ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) (union p q) >>= ⊂:-overloads-right
  step (F ∩ G) (union (left (right p)) (left (left q))) = ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) (union q p) >>= ⊂:-overloads-right >>=ˡ <:-∪-symm >>=ʳ <:-∪-symm
  step (F ∩ G) (union (left (right p)) (left (right q))) = step G (union p q) >>= ⊂:-overloads-right >>= ⊂:-overloads-left
  step (F ∩ G) (union p (right q)) with ⊂:-⋓-overloads (normal-∪-saturate F) (normal-∪-saturate G) q
  step (F ∩ G) (union (left (left p)) (right q)) | defn (union q₁ q₂) q₃ q₄ =
    (step F (union p q₁) [∪] just q₂) >>=
    ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-union <:-refl q₃) <:-∪-assocl >>=ʳ
    <:-trans <:-∪-assocr (<:-union <:-refl q₄)
  step (F ∩ G) (union (left (right p)) (right q)) | defn (union q₁ q₂) q₃ q₄ =
    (just q₁ [∪] step G (union p q₂)) >>=
    ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-union <:-refl q₃) (<:-∪-lub (<:-trans <:-∪-left <:-∪-right) (<:-∪-lub <:-∪-left (<:-trans <:-∪-right <:-∪-right))) >>=ʳ
    <:-trans (<:-∪-lub (<:-trans <:-∪-left <:-∪-right) (<:-∪-lub <:-∪-left (<:-trans <:-∪-right <:-∪-right))) (<:-union <:-refl q₄)
  step (F ∩ G) (union (right p) (right q)) | defn (union q₁ q₂) q₃ q₄ with ⊂:-⋓-overloads (normal-∪-saturate F) (normal-∪-saturate G) p
  step (F ∩ G) (union (right p) (right q)) | defn (union q₁ q₂) q₃ q₄ | defn (union p₁ p₂) p₃ p₄ =
    (step F (union p₁ q₁) [∪] step G (union p₂ q₂)) >>=
    ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-union p₃ q₃) (<:-∪-lub (<:-union <:-∪-left <:-∪-left) (<:-union <:-∪-right <:-∪-right)) >>=ʳ
    <:-trans (<:-∪-lub (<:-union <:-∪-left <:-∪-left) (<:-union <:-∪-right <:-∪-right)) (<:-union p₄ q₄)
  step (F ∩ G) (union (right p) q) with ⊂:-⋓-overloads (normal-∪-saturate F) (normal-∪-saturate G) p
  step (F ∩ G) (union (right p) (left (left q))) | defn (union p₁ p₂) p₃ p₄ =
    (step F (union p₁ q) [∪] just p₂) >>=
    ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-union p₃ <:-refl) (<:-∪-lub (<:-union <:-∪-left <:-refl) (<:-trans <:-∪-right <:-∪-left)) >>=ʳ
    <:-trans (<:-∪-lub (<:-union <:-∪-left <:-refl) (<:-trans <:-∪-right <:-∪-left)) (<:-union p₄ <:-refl)
  step (F ∩ G) (union (right p) (left (right q))) | defn (union p₁ p₂) p₃ p₄ =
    (just p₁ [∪] step G (union p₂ q)) >>=
    ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-union p₃ <:-refl) <:-∪-assocr >>=ʳ
    <:-trans <:-∪-assocl (<:-union p₄ <:-refl)
  step (F ∩ G) (union (right p) (right q)) | defn (union p₁ p₂) p₃ p₄ with ⊂:-⋓-overloads (normal-∪-saturate F) (normal-∪-saturate G) q
  step (F ∩ G) (union (right p) (right q)) | defn (union p₁ p₂) p₃ p₄ | defn (union q₁ q₂) q₃ q₄ =
    (step F (union p₁ q₁) [∪] step G (union p₂ q₂)) >>=
    ⊂:-overloads-⋓ (normal-∪-saturate F) (normal-∪-saturate G) >>=
    ⊂:-overloads-right >>=ˡ
    <:-trans (<:-union p₃ q₃) (<:-∪-lub (<:-union <:-∪-left <:-∪-left) (<:-union <:-∪-right <:-∪-right)) >>=ʳ
    <:-trans (<:-∪-lub (<:-union <:-∪-left <:-∪-left) (<:-union <:-∪-right <:-∪-right)) (<:-union p₄ q₄)

∪-saturated : ∀ {F} → FunType F → ∪-Lift (Overloads (∪-saturate F)) (Overloads (∪-saturate F)) ⊂: Overloads (∪-saturate F)
∪-saturated F o =
  ⊂:-∪-lift (∪-saturate-overloads F) (∪-saturate-overloads F) o >>=
  ⊂:-∪-lift-saturate >>=
  overloads-∪-saturate F

∩-saturate-overloads : ∀ {F} → FunType F → Overloads (∩-saturate F) ⊂: ∩-Saturate (Overloads F)
∩-saturate-overloads (S ⇒ T) here = just (base here)
∩-saturate-overloads (F ∩ G) (left (left o)) = ∩-saturate-overloads F o >>= ⊂:-∩-saturate ⊂:-overloads-left
∩-saturate-overloads (F ∩ G) (left (right o)) = ∩-saturate-overloads G o >>= ⊂:-∩-saturate ⊂:-overloads-right
∩-saturate-overloads (F ∩ G) (right o) =
  ⊂:-⋒-overloads (normal-∩-saturate F) (normal-∩-saturate G) o >>=
  ⊂:-∩-lift (∩-saturate-overloads F) (∩-saturate-overloads G) >>=
  ⊂:-∩-lift (⊂:-∩-saturate ⊂:-overloads-left) (⊂:-∩-saturate ⊂:-overloads-right) >>=
  ⊂:-∩-lift-saturate

overloads-∩-saturate : ∀ {F} → FunType F → ∩-Saturate (Overloads F) ⊂: Overloads (∩-saturate F)
overloads-∩-saturate F = ⊂:-∩-saturate-indn (inj F) (step F) where
  
  inj :  ∀ {F} → FunType F → Overloads F ⊂: Overloads (∩-saturate F)
  inj (S ⇒ T) here = just here
  inj (F ∩ G) (left p) = inj F p >>= ⊂:-overloads-left >>= ⊂:-overloads-left
  inj (F ∩ G) (right p) = inj G p >>= ⊂:-overloads-right >>= ⊂:-overloads-left

  step : ∀ {F} → FunType F → ∩-Lift (Overloads (∩-saturate F)) (Overloads (∩-saturate F)) ⊂: Overloads (∩-saturate F)
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

saturate-overloads : ∀ {F} → FunType F → Overloads (saturate F) ⊂: ∪-Saturate (∩-Saturate (Overloads F))
saturate-overloads F o = ∪-saturate-overloads (normal-∩-saturate F) o >>= (⊂:-∪-saturate (∩-saturate-overloads F))

overloads-saturate : ∀ {F} → FunType F → ∪-Saturate (∩-Saturate (Overloads F)) ⊂: Overloads (saturate F)
overloads-saturate F o = ⊂:-∪-saturate (overloads-∩-saturate F) o >>= overloads-∪-saturate (normal-∩-saturate F)

-- Saturated F whenever
-- * if F has overloads (R ⇒ S) and (T ⇒ U) then F has an overload which is a subtype of ((R ∩ T) ⇒ (S ∩ U))
-- * ditto union
data Saturated (F : Type) : Set where

  defn : 

    (∀ {R S T U} → Overloads F (R ⇒ S) → Overloads F (T ⇒ U) → <:-Close (Overloads F) ((R ∩ T) ⇒ (S ∩ U))) →
    (∀ {R S T U} → Overloads F (R ⇒ S) → Overloads F (T ⇒ U) → <:-Close (Overloads F) ((R ∪ T) ⇒ (S ∪ U))) →
    -----------
    Saturated F

-- saturated F is saturated!
saturated : ∀ {F} → FunType F → Saturated (saturate F)
saturated F = defn
  (λ n o → (saturate-overloads F n [∩] saturate-overloads F o) >>= ∪-saturate-resp-∩-saturation ⊂:-∩-lift-saturate >>= overloads-saturate F)
  (λ n o → ∪-saturated (normal-∩-saturate F) (union n o))
