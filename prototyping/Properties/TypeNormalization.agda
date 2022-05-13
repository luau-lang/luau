{-# OPTIONS --rewriting #-}

module Properties.TypeNormalization where

open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_)
open import Luau.Subtyping using (Tree; Language; function; scalar; unknown; right; scalar-function-err; _,_)
open import Luau.TypeNormalization using (_∪ⁿ_; _∩ⁿ_; _∪ᶠ_; _∪ⁿˢ_; _∩ⁿˢ_; _⇒ⁿ_; tgtⁿ; normalize)
open import Luau.Subtyping using (_<:_)
open import Properties.Subtyping using (<:-trans; <:-refl; <:-unknown; <:-never; <:-∪-left; <:-∪-right; <:-∪-lub; <:-∩-left; <:-∩-right; <:-∩-glb; <:-∩-symm; <:-function; <:-function-∪-∩; <:-function-∩-∪; <:-function-∪; <:-everything; <:-union; <:-∪-assocl; <:-∪-assocr; <:-∪-symm; <:-intersect;  ∪-distl-∩-<:; ∪-distr-∩-<:; <:-∪-distr-∩; <:-∪-distl-∩; ∩-distl-∪-<:; <:-∩-distl-∪; <:-∩-distr-∪; scalar-∩-function-<:-never; scalar-≢-∩-<:-never; <:-function-never)

-- Normal forms for types
data FunType : Type → Set
data Inhabited : Type → Set
data Normal : Type → Set

data FunType where
  function : FunType (never ⇒ unknown)
  _⇒_ : ∀ {S T} → Inhabited S → Normal T → FunType (S ⇒ T)
  _∩_ : ∀ {F G} → FunType F → FunType G → FunType (F ∩ G)

data Inhabited where
  function : Inhabited (never ⇒ unknown)
  _⇒_ : ∀ {S T} → Inhabited S → Normal T → Inhabited (S ⇒ T)
  _∩_ : ∀ {F G} → FunType F → FunType G → Inhabited (F ∩ G)
  _∪_ : ∀ {S T} → Normal S → Scalar T → Inhabited (S ∪ T)
  unknown : Inhabited unknown

data Normal where
  function : Normal (never ⇒ unknown) 
  _⇒_ : ∀ {S T} → Inhabited S → Normal T → Normal (S ⇒ T)
  _∩_ : ∀ {F G} → FunType F → FunType G → Normal (F ∩ G)
  _∪_ : ∀ {S T} → Normal S → Scalar T → Normal (S ∪ T)
  never : Normal never
  unknown : Normal unknown

data OptScalar : Type → Set where
  never : OptScalar never
  number : OptScalar number
  boolean : OptScalar boolean
  string : OptScalar string
  nil : OptScalar nil

-- Function types are inhabited
inhabitedᶠ : ∀ {F} → (FunType F) → Language F function
inhabitedᶠ function = function
inhabitedᶠ (S ⇒ T) = function
inhabitedᶠ (F ∩ G) = (inhabitedᶠ F , inhabitedᶠ G)

-- Inhabited types are inhabited
inhabitant : ∀ {T} → Inhabited T → Tree
inhabitant function = function
inhabitant (S ⇒ T) = function
inhabitant (S ∩ T) = function
inhabitant (S ∪ T) = scalar T
inhabitant unknown = function

inhabited : ∀ {T} → (Tⁱ : Inhabited T) → Language T (inhabitant Tⁱ)
inhabited function = function
inhabited (S ⇒ T) = function
inhabited (S ∩ T) = inhabitedᶠ (S ∩ T)
inhabited (S ∪ T) = right (scalar T)
inhabited unknown = unknown

-- Top function type
function-top : ∀ {F} → (FunType F) → (F <: (never ⇒ unknown))
function-top function = <:-refl
function-top (S ⇒ T) = <:-function <:-never <:-unknown
function-top (F ∩ G) = <:-trans <:-∩-left (function-top F)

-- Normalization produces normal types
normal : ∀ T → Normal (normalize T)
normalᶠ : ∀ {F} → FunType F → Normal F
normalⁱ : ∀ {T} → Inhabited T → Normal T
normal-∪ⁿ : ∀ {S T} → Normal S → Normal T → Normal (S ∪ⁿ T)
normal-∩ⁿ : ∀ {S T} → Normal S → Normal T → Normal (S ∩ⁿ T)
normal-∪ⁿˢ : ∀ {S T} → Normal S → OptScalar T → Normal (S ∪ⁿˢ T)
normal-∩ⁿˢ : ∀ {S T} → Normal S → Scalar T → OptScalar (S ∩ⁿˢ T)
normal-∪ᶠ : ∀ {F G} → FunType F → FunType G → FunType (F ∪ᶠ G)
normal-⇒ⁿ : ∀ {S T} → Normal S → Normal T → FunType (S ⇒ⁿ T)

normal nil = never ∪ nil
normal (S ⇒ T) = normalᶠ (normal-⇒ⁿ (normal S) (normal T))
normal never = never
normal unknown = unknown
normal boolean = never ∪ boolean
normal number = never ∪ number
normal string = never ∪ string
normal (S ∪ T) = normal-∪ⁿ (normal S) (normal T)
normal (S ∩ T) = normal-∩ⁿ (normal S) (normal T)

normalᶠ function = function
normalᶠ (S ⇒ T) = S ⇒ T
normalᶠ (F ∩ G) = F ∩ G

normalⁱ function = function
normalⁱ (S ⇒ T) = S ⇒ T
normalⁱ (S ∩ T) = S ∩ T
normalⁱ (S ∪ T) = S ∪ T
normalⁱ unknown = unknown

normal-∪ⁿ S (T₁ ∪ T₂) = (normal-∪ⁿ S T₁) ∪ T₂
normal-∪ⁿ S never = S
normal-∪ⁿ S unknown = unknown
normal-∪ⁿ never (T ⇒ U) = T ⇒ U
normal-∪ⁿ never (G₁ ∩ G₂) = G₁ ∩ G₂
normal-∪ⁿ unknown (T ⇒ U) = unknown
normal-∪ⁿ unknown (G₁ ∩ G₂) = unknown
normal-∪ⁿ (R ⇒ S) (T ⇒ U) = normalᶠ (normal-∪ᶠ (R ⇒ S) (T ⇒ U))
normal-∪ⁿ (R ⇒ S) (G₁ ∩ G₂) = normalᶠ (normal-∪ᶠ (R ⇒ S) (G₁ ∩ G₂))
normal-∪ⁿ (F₁ ∩ F₂) (T ⇒ U) = normalᶠ (normal-∪ᶠ (F₁ ∩ F₂) (T ⇒ U))
normal-∪ⁿ (F₁ ∩ F₂) (G₁ ∩ G₂) = normalᶠ (normal-∪ᶠ (F₁ ∩ F₂) (G₁ ∩ G₂))
normal-∪ⁿ (S₁ ∪ S₂) (T₁ ⇒ T₂) = normal-∪ⁿ S₁ (T₁ ⇒ T₂) ∪ S₂
normal-∪ⁿ (S₁ ∪ S₂) (G₁ ∩ G₂) = normal-∪ⁿ S₁ (G₁ ∩ G₂) ∪ S₂
normal-∪ⁿ function function = function
normal-∪ⁿ (R ⇒ S) function = function
normal-∪ⁿ (R ∩ S) function = (normal-∪ᶠ R function) ∩ (normal-∪ᶠ S function)
normal-∪ⁿ (R ∪ S) function = normal-∪ⁿ R function ∪ S
normal-∪ⁿ never function = function
normal-∪ⁿ unknown function = unknown
normal-∪ⁿ function (T ⇒ U) = normalᶠ (normal-⇒ⁿ (normal-∩ⁿ never (normalⁱ T)) (normal-∪ⁿ unknown U))
normal-∪ⁿ function (T ∩ U) = normal-∪ᶠ function T ∩ normal-∪ᶠ function U

normal-∩ⁿ S never = never
normal-∩ⁿ S unknown = S
normal-∩ⁿ S (T ∪ U) = normal-∪ⁿˢ (normal-∩ⁿ S T) (normal-∩ⁿˢ S U )
normal-∩ⁿ never (T ⇒ U) = never
normal-∩ⁿ unknown (T ⇒ U) = T ⇒ U
normal-∩ⁿ (R ⇒ S) (T ⇒ U) = (R ⇒ S) ∩ (T ⇒ U)
normal-∩ⁿ (R ∩ S) (T ⇒ U) = (R ∩ S) ∩ (T ⇒ U)
normal-∩ⁿ (R ∪ S) (T ⇒ U) = normal-∩ⁿ R (T ⇒ U)
normal-∩ⁿ never (T ∩ U) = never
normal-∩ⁿ unknown (T ∩ U) = T ∩ U
normal-∩ⁿ (R ⇒ S) (T ∩ U) = (R ⇒ S) ∩ (T ∩ U)
normal-∩ⁿ (R ∩ S) (T ∩ U) = (R ∩ S) ∩ (T ∩ U)
normal-∩ⁿ (R ∪ S) (T ∩ U) = normal-∩ⁿ R (T ∩ U)
normal-∩ⁿ function function = function ∩ function
normal-∩ⁿ (R ⇒ S) function = (R ⇒ S) ∩ function
normal-∩ⁿ (R ∩ S) function = (R ∩ S) ∩ function
normal-∩ⁿ (R ∪ S) function = normal-∩ⁿ R function
normal-∩ⁿ never function = never
normal-∩ⁿ unknown function = function
normal-∩ⁿ function (T ⇒ U) = function ∩ (T ⇒ U)
normal-∩ⁿ function (T ∩ U) = function ∩ (T ∩ U)

normal-∪ⁿˢ S never = S
normal-∪ⁿˢ never number = never ∪ number
normal-∪ⁿˢ unknown number = unknown
normal-∪ⁿˢ (R ⇒ S) number = (R ⇒ S) ∪ number
normal-∪ⁿˢ (R ∩ S) number = (R ∩ S) ∪ number
normal-∪ⁿˢ (R ∪ number) number = R ∪ number
normal-∪ⁿˢ (R ∪ boolean) number = normal-∪ⁿˢ R number ∪ boolean
normal-∪ⁿˢ (R ∪ string) number = normal-∪ⁿˢ R number ∪ string
normal-∪ⁿˢ (R ∪ nil) number = normal-∪ⁿˢ R number ∪ nil
normal-∪ⁿˢ never boolean = never ∪ boolean
normal-∪ⁿˢ unknown boolean = unknown
normal-∪ⁿˢ (R ⇒ S) boolean = (R ⇒ S) ∪ boolean
normal-∪ⁿˢ (R ∩ S) boolean = (R ∩ S) ∪ boolean
normal-∪ⁿˢ (R ∪ number) boolean = normal-∪ⁿˢ R boolean ∪ number
normal-∪ⁿˢ (R ∪ boolean) boolean = R ∪ boolean
normal-∪ⁿˢ (R ∪ string) boolean = normal-∪ⁿˢ R boolean ∪ string
normal-∪ⁿˢ (R ∪ nil) boolean = normal-∪ⁿˢ R boolean ∪ nil
normal-∪ⁿˢ never string = never ∪ string
normal-∪ⁿˢ unknown string = unknown
normal-∪ⁿˢ (R ⇒ S) string = (R ⇒ S) ∪ string
normal-∪ⁿˢ (R ∩ S) string = (R ∩ S) ∪ string
normal-∪ⁿˢ (R ∪ number) string = normal-∪ⁿˢ R string ∪ number
normal-∪ⁿˢ (R ∪ boolean) string = normal-∪ⁿˢ R string ∪ boolean
normal-∪ⁿˢ (R ∪ string) string = R ∪ string
normal-∪ⁿˢ (R ∪ nil) string = normal-∪ⁿˢ R string ∪ nil
normal-∪ⁿˢ never nil = never ∪ nil
normal-∪ⁿˢ unknown nil = unknown
normal-∪ⁿˢ (R ⇒ S) nil = (R ⇒ S) ∪ nil
normal-∪ⁿˢ (R ∩ S) nil = (R ∩ S) ∪ nil
normal-∪ⁿˢ (R ∪ number) nil = normal-∪ⁿˢ R nil ∪ number
normal-∪ⁿˢ (R ∪ boolean) nil = normal-∪ⁿˢ R nil ∪ boolean
normal-∪ⁿˢ (R ∪ string) nil = normal-∪ⁿˢ R nil ∪ string
normal-∪ⁿˢ (R ∪ nil) nil = R ∪ nil
normal-∪ⁿˢ function number = function ∪ number
normal-∪ⁿˢ function boolean = function ∪ boolean
normal-∪ⁿˢ function string = function ∪ string
normal-∪ⁿˢ function nil = function ∪ nil

normal-∩ⁿˢ never number = never
normal-∩ⁿˢ never boolean = never
normal-∩ⁿˢ never string = never
normal-∩ⁿˢ never nil = never
normal-∩ⁿˢ unknown number = number
normal-∩ⁿˢ unknown boolean = boolean
normal-∩ⁿˢ unknown string = string
normal-∩ⁿˢ unknown nil = nil
normal-∩ⁿˢ (R ⇒ S) number = never
normal-∩ⁿˢ (R ⇒ S) boolean = never
normal-∩ⁿˢ (R ⇒ S) string = never
normal-∩ⁿˢ (R ⇒ S) nil = never
normal-∩ⁿˢ (R ∩ S) number = never
normal-∩ⁿˢ (R ∩ S) boolean = never
normal-∩ⁿˢ (R ∩ S) string = never
normal-∩ⁿˢ (R ∩ S) nil = never
normal-∩ⁿˢ (R ∪ number) number = number
normal-∩ⁿˢ (R ∪ boolean) number = normal-∩ⁿˢ R number
normal-∩ⁿˢ (R ∪ string) number = normal-∩ⁿˢ R number
normal-∩ⁿˢ (R ∪ nil) number = normal-∩ⁿˢ R number
normal-∩ⁿˢ (R ∪ number) boolean = normal-∩ⁿˢ R boolean
normal-∩ⁿˢ (R ∪ boolean) boolean = boolean
normal-∩ⁿˢ (R ∪ string) boolean = normal-∩ⁿˢ R boolean
normal-∩ⁿˢ (R ∪ nil) boolean = normal-∩ⁿˢ R boolean
normal-∩ⁿˢ (R ∪ number) string = normal-∩ⁿˢ R string
normal-∩ⁿˢ (R ∪ boolean) string = normal-∩ⁿˢ R string
normal-∩ⁿˢ (R ∪ string) string = string
normal-∩ⁿˢ (R ∪ nil) string = normal-∩ⁿˢ R string
normal-∩ⁿˢ (R ∪ number) nil = normal-∩ⁿˢ R nil
normal-∩ⁿˢ (R ∪ boolean) nil = normal-∩ⁿˢ R nil
normal-∩ⁿˢ (R ∪ string) nil = normal-∩ⁿˢ R nil
normal-∩ⁿˢ (R ∪ nil) nil = nil
normal-∩ⁿˢ function number = never
normal-∩ⁿˢ function boolean = never
normal-∩ⁿˢ function string = never
normal-∩ⁿˢ function nil = never

normal-∪ᶠ (R ⇒ S) (T ⇒ U) = normal-⇒ⁿ (normal-∩ⁿ (normalⁱ R) (normalⁱ T)) (normal-∪ⁿ S U)
normal-∪ᶠ (R ⇒ S) (G ∩ H) = normal-∪ᶠ (R ⇒ S) G ∩ normal-∪ᶠ (R ⇒ S) H
normal-∪ᶠ (E ∩ F) G = normal-∪ᶠ E G ∩ normal-∪ᶠ F G
normal-∪ᶠ function function = function
normal-∪ᶠ function (T ⇒ U) = normal-⇒ⁿ (normal-∩ⁿ never (normalⁱ T)) (normal-∪ⁿ unknown U)
normal-∪ᶠ function (G ∩ H) = normal-∪ᶠ function G ∩ normal-∪ᶠ function H
normal-∪ᶠ (R ⇒ S) function = function

normal-⇒ⁿ function T = function ⇒ T
normal-⇒ⁿ (R ⇒ S) T = (R ⇒ S) ⇒ T
normal-⇒ⁿ (R ∩ S) T = (R ∩ S) ⇒ T
normal-⇒ⁿ (R ∪ S) T = (R ∪ S) ⇒ T
normal-⇒ⁿ never T = function
normal-⇒ⁿ unknown T = unknown ⇒ T

scalar-∩-fun-<:-never : ∀ {F S} → FunType F → Scalar S → (F ∩ S) <: never
scalar-∩-fun-<:-never function S = scalar-∩-function-<:-never S
scalar-∩-fun-<:-never (T ⇒ U) S = scalar-∩-function-<:-never S
scalar-∩-fun-<:-never (F ∩ G) S = <:-trans (<:-intersect <:-∩-left <:-refl) (scalar-∩-fun-<:-never F S)

<:-tgtⁿ : ∀ {S T U} → (T <: U) → T <: tgtⁿ S U
<:-tgtⁿ {never} p = <:-unknown
<:-tgtⁿ {nil} p = p
<:-tgtⁿ {unknown} p = p
<:-tgtⁿ {boolean} p = p
<:-tgtⁿ {number} p = p
<:-tgtⁿ {string} p = p
<:-tgtⁿ {S ⇒ T} p = p
<:-tgtⁿ {S ∪ T} p = p
<:-tgtⁿ {S ∩ T} p = p

flipper : ∀ {S T U} → ((S ∪ T) ∪ U) <: ((S ∪ U) ∪ T)
flipper = <:-trans <:-∪-assocr (<:-trans (<:-union <:-refl <:-∪-symm) <:-∪-assocl)

∩-<:-∩ⁿ :  ∀ {S T} → Normal S → Normal T → (S ∩ T) <: (S ∩ⁿ T)
∩ⁿ-<:-∩ :  ∀ {S T} → Normal S → Normal T → (S ∩ⁿ T) <: (S ∩ T)
∩-<:-∩ⁿˢ :  ∀ {S T} → Normal S → Scalar T → (S ∩ T) <: (S ∩ⁿˢ T)
∩ⁿˢ-<:-∩ :  ∀ {S T} → Normal S → Scalar T → (S ∩ⁿˢ T) <: (S ∩ T)
∪ᶠ-<:-∪ : ∀ {F G} → FunType F → FunType G → (F ∪ᶠ G) <: (F ∪ G)
∪ⁿ-<:-∪ : ∀ {S T} → Normal S → Normal T → (S ∪ⁿ T) <: (S ∪ T)
∪-<:-∪ⁿ : ∀ {S T} → Normal S → Normal T → (S ∪ T) <: (S ∪ⁿ T)
∪ⁿˢ-<:-∪ : ∀ {S T} → Normal S → OptScalar T → (S ∪ⁿˢ T) <: (S ∪ T)
∪-<:-∪ⁿˢ : ∀ {S T} → Normal S → OptScalar T → (S ∪ T) <: (S ∪ⁿˢ T)
⇒-<:-⇒ⁿ : ∀ {S T} → Normal S → Normal T → (S ⇒ T) <: (S ⇒ⁿ T)
⇒ⁿ-<:-⇒ : ∀ {S T} → Normal S → Normal T → (S ⇒ⁿ T) <: (S ⇒ T)

∩-<:-∩ⁿ S never = <:-∩-right
∩-<:-∩ⁿ S unknown = <:-∩-left
∩-<:-∩ⁿ S (T ∪ U) = <:-trans <:-∩-distl-∪ (<:-trans (<:-union (∩-<:-∩ⁿ S T) (∩-<:-∩ⁿˢ S U)) (∪-<:-∪ⁿˢ (normal-∩ⁿ S T) (normal-∩ⁿˢ S U)) )
∩-<:-∩ⁿ never (T ⇒ U) = <:-∩-left
∩-<:-∩ⁿ unknown (T ⇒ U) = <:-∩-right
∩-<:-∩ⁿ (R ⇒ S) (T ⇒ U) = <:-refl
∩-<:-∩ⁿ (R ∩ S) (T ⇒ U) = <:-refl
∩-<:-∩ⁿ (R ∪ S) (T ⇒ U) = <:-trans <:-∩-distr-∪ (<:-trans (<:-union (∩-<:-∩ⁿ R (T ⇒ U)) (<:-trans <:-∩-symm (∩-<:-∩ⁿˢ (T ⇒ U) S))) (<:-∪-lub <:-refl <:-never))
∩-<:-∩ⁿ never (T ∩ U) = <:-∩-left
∩-<:-∩ⁿ unknown (T ∩ U) = <:-∩-right
∩-<:-∩ⁿ (R ⇒ S) (T ∩ U) = <:-refl
∩-<:-∩ⁿ (R ∩ S) (T ∩ U) = <:-refl
∩-<:-∩ⁿ (R ∪ S) (T ∩ U) = <:-trans <:-∩-distr-∪ (<:-trans (<:-union (∩-<:-∩ⁿ R (T ∩ U)) (<:-trans <:-∩-symm (∩-<:-∩ⁿˢ (T ∩ U) S))) (<:-∪-lub <:-refl <:-never))
∩-<:-∩ⁿ function function = <:-refl
∩-<:-∩ⁿ function (T ⇒ U) = <:-refl
∩-<:-∩ⁿ function (T ∩ U) = <:-refl
∩-<:-∩ⁿ (R ⇒ S) function = <:-refl
∩-<:-∩ⁿ (R ∩ S) function = <:-refl
∩-<:-∩ⁿ (R ∪ S) function = <:-trans <:-∩-distr-∪ (<:-trans (<:-union (∩-<:-∩ⁿ R function) (<:-trans <:-∩-symm (∩-<:-∩ⁿˢ function S))) (<:-∪-lub <:-refl <:-never))
∩-<:-∩ⁿ never function = <:-∩-left
∩-<:-∩ⁿ unknown function = <:-∩-right

∩ⁿ-<:-∩ S never = <:-never
∩ⁿ-<:-∩ S unknown = <:-∩-glb <:-refl <:-unknown
∩ⁿ-<:-∩ S (T ∪ U) = <:-trans (∪ⁿˢ-<:-∪ (normal-∩ⁿ S T) (normal-∩ⁿˢ S U)) (<:-trans (<:-union (∩ⁿ-<:-∩ S T) (∩ⁿˢ-<:-∩ S U)) ∩-distl-∪-<:)
∩ⁿ-<:-∩ never (T ⇒ U) = <:-never
∩ⁿ-<:-∩ unknown (T ⇒ U) = <:-∩-glb <:-unknown <:-refl
∩ⁿ-<:-∩ (R ⇒ S) (T ⇒ U) = <:-refl
∩ⁿ-<:-∩ (R ∩ S) (T ⇒ U) = <:-refl
∩ⁿ-<:-∩ (R ∪ S) (T ⇒ U) = <:-trans (∩ⁿ-<:-∩ R (T ⇒ U)) (<:-∩-glb (<:-trans <:-∩-left <:-∪-left) <:-∩-right)
∩ⁿ-<:-∩ never (T ∩ U) = <:-never
∩ⁿ-<:-∩ unknown (T ∩ U) = <:-∩-glb <:-unknown <:-refl
∩ⁿ-<:-∩ (R ⇒ S) (T ∩ U) = <:-refl
∩ⁿ-<:-∩ (R ∩ S) (T ∩ U) = <:-refl
∩ⁿ-<:-∩ (R ∪ S) (T ∩ U) = <:-trans (∩ⁿ-<:-∩ R (T ∩ U)) (<:-∩-glb (<:-trans <:-∩-left <:-∪-left) <:-∩-right)
∩ⁿ-<:-∩ function function = <:-refl
∩ⁿ-<:-∩ function (S ⇒ T) = <:-refl
∩ⁿ-<:-∩ function (S ∩ T) = <:-refl
∩ⁿ-<:-∩ (R ⇒ S) function = <:-refl
∩ⁿ-<:-∩ (R ∩ S) function = <:-refl
∩ⁿ-<:-∩ (R ∪ S) function = <:-trans (∩ⁿ-<:-∩ R function) (<:-∩-glb (<:-trans <:-∩-left <:-∪-left) <:-∩-right)
∩ⁿ-<:-∩ never function = <:-never
∩ⁿ-<:-∩ unknown function = <:-∩-glb <:-unknown <:-refl

∩-<:-∩ⁿˢ never number = <:-∩-left
∩-<:-∩ⁿˢ never boolean = <:-∩-left
∩-<:-∩ⁿˢ never string = <:-∩-left
∩-<:-∩ⁿˢ never nil = <:-∩-left
∩-<:-∩ⁿˢ unknown T = <:-∩-right
∩-<:-∩ⁿˢ (R ⇒ S) T = scalar-∩-fun-<:-never (R ⇒ S) T
∩-<:-∩ⁿˢ (F ∩ G) T = scalar-∩-fun-<:-never (F ∩ G) T
∩-<:-∩ⁿˢ (R ∪ number) number = <:-∩-right
∩-<:-∩ⁿˢ (R ∪ boolean) number = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R number) (scalar-≢-∩-<:-never boolean number (λ ())))
∩-<:-∩ⁿˢ (R ∪ string) number = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R number) (scalar-≢-∩-<:-never string number (λ ())))
∩-<:-∩ⁿˢ (R ∪ nil) number = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R number) (scalar-≢-∩-<:-never nil number (λ ())))
∩-<:-∩ⁿˢ (R ∪ number) boolean = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R boolean) (scalar-≢-∩-<:-never number boolean (λ ())))
∩-<:-∩ⁿˢ (R ∪ boolean) boolean = <:-∩-right
∩-<:-∩ⁿˢ (R ∪ string) boolean = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R boolean) (scalar-≢-∩-<:-never string boolean (λ ())))
∩-<:-∩ⁿˢ (R ∪ nil) boolean = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R boolean) (scalar-≢-∩-<:-never nil boolean (λ ())))
∩-<:-∩ⁿˢ (R ∪ number) string = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R string) (scalar-≢-∩-<:-never number string (λ ())))
∩-<:-∩ⁿˢ (R ∪ boolean) string = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R string) (scalar-≢-∩-<:-never boolean string (λ ())))
∩-<:-∩ⁿˢ (R ∪ string) string = <:-∩-right
∩-<:-∩ⁿˢ (R ∪ nil) string = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R string) (scalar-≢-∩-<:-never nil string (λ ())))
∩-<:-∩ⁿˢ (R ∪ number) nil = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R nil) (scalar-≢-∩-<:-never number nil (λ ())))
∩-<:-∩ⁿˢ (R ∪ boolean) nil = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R nil) (scalar-≢-∩-<:-never boolean nil (λ ())))
∩-<:-∩ⁿˢ (R ∪ string) nil = <:-trans <:-∩-distr-∪ (<:-∪-lub (∩-<:-∩ⁿˢ R nil) (scalar-≢-∩-<:-never string nil (λ ())))
∩-<:-∩ⁿˢ (R ∪ nil) nil = <:-∩-right
∩-<:-∩ⁿˢ function T = scalar-∩-fun-<:-never function T

∩ⁿˢ-<:-∩ never T = <:-never
∩ⁿˢ-<:-∩ unknown T = <:-∩-glb <:-unknown <:-refl
∩ⁿˢ-<:-∩ (R ⇒ S) T = <:-never
∩ⁿˢ-<:-∩ (F ∩ G) T = <:-never
∩ⁿˢ-<:-∩ (R ∪ number) number = <:-∩-glb <:-∪-right <:-refl
∩ⁿˢ-<:-∩ (R ∪ boolean) number = <:-trans (∩ⁿˢ-<:-∩ R number) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ string) number = <:-trans (∩ⁿˢ-<:-∩ R number) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ nil) number = <:-trans (∩ⁿˢ-<:-∩ R number) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ number) boolean = <:-trans (∩ⁿˢ-<:-∩ R boolean) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ boolean) boolean = <:-∩-glb <:-∪-right <:-refl
∩ⁿˢ-<:-∩ (R ∪ string) boolean = <:-trans (∩ⁿˢ-<:-∩ R boolean) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ nil) boolean = <:-trans (∩ⁿˢ-<:-∩ R boolean) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ number) string = <:-trans (∩ⁿˢ-<:-∩ R string) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ boolean) string = <:-trans (∩ⁿˢ-<:-∩ R string) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ string) string = <:-∩-glb <:-∪-right <:-refl
∩ⁿˢ-<:-∩ (R ∪ nil) string = <:-trans (∩ⁿˢ-<:-∩ R string) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ number) nil = <:-trans (∩ⁿˢ-<:-∩ R nil) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ boolean) nil = <:-trans (∩ⁿˢ-<:-∩ R nil) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ string) nil = <:-trans (∩ⁿˢ-<:-∩ R nil) (<:-intersect <:-∪-left <:-refl)
∩ⁿˢ-<:-∩ (R ∪ nil) nil = <:-∩-glb <:-∪-right <:-refl
∩ⁿˢ-<:-∩ function T = <:-never

∪ᶠ-<:-∪ (R ⇒ S) (T ⇒ U) = <:-trans (⇒ⁿ-<:-⇒ (normal-∩ⁿ (normalⁱ R) (normalⁱ T)) (normal-∪ⁿ S U)) (<:-trans (<:-function (∩-<:-∩ⁿ (normalⁱ R) (normalⁱ T)) (∪ⁿ-<:-∪ S U)) <:-function-∪-∩)
∪ᶠ-<:-∪ (R ⇒ S) (G ∩ H) = <:-trans (<:-intersect (∪ᶠ-<:-∪ (R ⇒ S) G) (∪ᶠ-<:-∪ (R ⇒ S) H)) ∪-distl-∩-<:
∪ᶠ-<:-∪ (E ∩ F) G = <:-trans (<:-intersect (∪ᶠ-<:-∪ E G) (∪ᶠ-<:-∪ F G)) ∪-distr-∩-<:
∪ᶠ-<:-∪ function function = <:-∪-left
∪ᶠ-<:-∪ function (T ⇒ U) = <:-trans (⇒ⁿ-<:-⇒ (normal-∩ⁿ never (normalⁱ T)) (normal-∪ⁿ unknown U)) (<:-trans (<:-function (∩-<:-∩ⁿ never (normalⁱ T)) (∪ⁿ-<:-∪ unknown U)) <:-function-∪-∩)
∪ᶠ-<:-∪ function (G ∩ H) = <:-trans (<:-intersect (∪ᶠ-<:-∪ function G) (∪ᶠ-<:-∪ function H)) ∪-distl-∩-<:
∪ᶠ-<:-∪ (R ⇒ S) function = <:-∪-right

∪-<:-∪ᶠ : ∀ {F G} → FunType F → FunType G → (F ∪ G) <: (F ∪ᶠ G)
∪-<:-∪ᶠ (R ⇒ S) (T ⇒ U) = <:-trans (<:-trans <:-function-∪ (<:-function (∩ⁿ-<:-∩ (normalⁱ R) (normalⁱ T)) (∪-<:-∪ⁿ S U))) (⇒-<:-⇒ⁿ (normal-∩ⁿ (normalⁱ R) (normalⁱ T)) (normal-∪ⁿ S U))
∪-<:-∪ᶠ (R ⇒ S) (G ∩ H) = <:-trans <:-∪-distl-∩ (<:-intersect (∪-<:-∪ᶠ (R ⇒ S) G) (∪-<:-∪ᶠ (R ⇒ S) H))
∪-<:-∪ᶠ (E ∩ F) G = <:-trans <:-∪-distr-∩ (<:-intersect (∪-<:-∪ᶠ E G) (∪-<:-∪ᶠ F G))
∪-<:-∪ᶠ function function = <:-∪-lub <:-refl <:-refl
∪-<:-∪ᶠ function (T ⇒ U) = <:-trans (<:-trans <:-function-∪ (<:-function (∩ⁿ-<:-∩ never (normalⁱ T)) (∪-<:-∪ⁿ unknown U))) (⇒-<:-⇒ⁿ (normal-∩ⁿ never (normalⁱ T)) (normal-∪ⁿ unknown U))
∪-<:-∪ᶠ function (G ∩ H) = <:-trans <:-∪-distl-∩ (<:-intersect (∪-<:-∪ᶠ function G) (∪-<:-∪ᶠ function H))
∪-<:-∪ᶠ (R ⇒ S) function = <:-∪-lub (<:-function <:-never <:-unknown) <:-refl

∪ⁿˢ-<:-∪ S never = <:-∪-left
∪ⁿˢ-<:-∪ never number = <:-refl
∪ⁿˢ-<:-∪ never boolean = <:-refl
∪ⁿˢ-<:-∪ never string = <:-refl
∪ⁿˢ-<:-∪ never nil = <:-refl
∪ⁿˢ-<:-∪ unknown number = <:-∪-left
∪ⁿˢ-<:-∪ unknown boolean = <:-∪-left
∪ⁿˢ-<:-∪ unknown string = <:-∪-left
∪ⁿˢ-<:-∪ unknown nil = <:-∪-left
∪ⁿˢ-<:-∪ (R ⇒ S) number = <:-refl
∪ⁿˢ-<:-∪ (R ⇒ S) boolean = <:-refl
∪ⁿˢ-<:-∪ (R ⇒ S) string = <:-refl
∪ⁿˢ-<:-∪ (R ⇒ S) nil = <:-refl
∪ⁿˢ-<:-∪ (R ∩ S) number = <:-refl
∪ⁿˢ-<:-∪ (R ∩ S) boolean = <:-refl
∪ⁿˢ-<:-∪ (R ∩ S) string = <:-refl
∪ⁿˢ-<:-∪ (R ∩ S) nil = <:-refl
∪ⁿˢ-<:-∪ (R ∪ number) number = <:-union <:-∪-left <:-refl
∪ⁿˢ-<:-∪ (R ∪ boolean) number = <:-trans (<:-union (∪ⁿˢ-<:-∪ R number) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ string) number = <:-trans (<:-union (∪ⁿˢ-<:-∪ R number) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ nil) number = <:-trans (<:-union (∪ⁿˢ-<:-∪ R number) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ number) boolean = <:-trans (<:-union (∪ⁿˢ-<:-∪ R boolean) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ boolean) boolean = <:-union <:-∪-left <:-refl
∪ⁿˢ-<:-∪ (R ∪ string) boolean = <:-trans (<:-union (∪ⁿˢ-<:-∪ R boolean) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ nil) boolean = <:-trans (<:-union (∪ⁿˢ-<:-∪ R boolean) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ number) string = <:-trans (<:-union (∪ⁿˢ-<:-∪ R string) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ boolean) string = <:-trans (<:-union (∪ⁿˢ-<:-∪ R string) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ string) string = <:-union <:-∪-left <:-refl
∪ⁿˢ-<:-∪ (R ∪ nil) string = <:-trans (<:-union (∪ⁿˢ-<:-∪ R string) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ number) nil = <:-trans (<:-union (∪ⁿˢ-<:-∪ R nil) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ boolean) nil = <:-trans (<:-union (∪ⁿˢ-<:-∪ R nil) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ string) nil = <:-trans (<:-union (∪ⁿˢ-<:-∪ R nil) <:-refl) flipper
∪ⁿˢ-<:-∪ (R ∪ nil) nil = <:-union <:-∪-left <:-refl
∪ⁿˢ-<:-∪ function number = <:-refl
∪ⁿˢ-<:-∪ function boolean = <:-refl
∪ⁿˢ-<:-∪ function string = <:-refl
∪ⁿˢ-<:-∪ function nil = <:-refl

∪-<:-∪ⁿˢ T never = <:-∪-lub <:-refl <:-never
∪-<:-∪ⁿˢ never number = <:-refl
∪-<:-∪ⁿˢ never boolean = <:-refl
∪-<:-∪ⁿˢ never string = <:-refl
∪-<:-∪ⁿˢ never nil = <:-refl
∪-<:-∪ⁿˢ unknown number = <:-unknown
∪-<:-∪ⁿˢ unknown boolean = <:-unknown
∪-<:-∪ⁿˢ unknown string = <:-unknown
∪-<:-∪ⁿˢ unknown nil = <:-unknown
∪-<:-∪ⁿˢ (R ⇒ S) number = <:-refl
∪-<:-∪ⁿˢ (R ⇒ S) boolean = <:-refl
∪-<:-∪ⁿˢ (R ⇒ S) string = <:-refl
∪-<:-∪ⁿˢ (R ⇒ S) nil = <:-refl
∪-<:-∪ⁿˢ (R ∩ S) number = <:-refl
∪-<:-∪ⁿˢ (R ∩ S) boolean = <:-refl
∪-<:-∪ⁿˢ (R ∩ S) string = <:-refl
∪-<:-∪ⁿˢ (R ∩ S) nil = <:-refl
∪-<:-∪ⁿˢ (R ∪ number) number = <:-∪-lub <:-refl <:-∪-right
∪-<:-∪ⁿˢ (R ∪ boolean) number = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R number) <:-refl)
∪-<:-∪ⁿˢ (R ∪ string) number = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R number) <:-refl)
∪-<:-∪ⁿˢ (R ∪ nil) number = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R number) <:-refl)
∪-<:-∪ⁿˢ (R ∪ number) boolean = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R boolean) <:-refl)
∪-<:-∪ⁿˢ (R ∪ boolean) boolean = <:-∪-lub <:-refl <:-∪-right
∪-<:-∪ⁿˢ (R ∪ string) boolean = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R boolean) <:-refl)
∪-<:-∪ⁿˢ (R ∪ nil) boolean = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R boolean) <:-refl)
∪-<:-∪ⁿˢ (R ∪ number) string = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R string) <:-refl)
∪-<:-∪ⁿˢ (R ∪ boolean) string = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R string) <:-refl)
∪-<:-∪ⁿˢ (R ∪ string) string = <:-∪-lub <:-refl <:-∪-right
∪-<:-∪ⁿˢ (R ∪ nil) string = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R string) <:-refl)
∪-<:-∪ⁿˢ (R ∪ number) nil = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R nil) <:-refl)
∪-<:-∪ⁿˢ (R ∪ boolean) nil = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R nil) <:-refl)
∪-<:-∪ⁿˢ (R ∪ string) nil = <:-trans flipper (<:-union (∪-<:-∪ⁿˢ R nil) <:-refl)
∪-<:-∪ⁿˢ (R ∪ nil) nil = <:-∪-lub <:-refl <:-∪-right
∪-<:-∪ⁿˢ function number = <:-refl
∪-<:-∪ⁿˢ function boolean = <:-refl
∪-<:-∪ⁿˢ function string = <:-refl
∪-<:-∪ⁿˢ function nil = <:-refl

∪ⁿ-<:-∪ S never = <:-∪-left
∪ⁿ-<:-∪ S unknown = <:-∪-right
∪ⁿ-<:-∪ never (T ⇒ U) = <:-∪-right
∪ⁿ-<:-∪ unknown (T ⇒ U) = <:-∪-left
∪ⁿ-<:-∪ (R ⇒ S) (T ⇒ U) = ∪ᶠ-<:-∪ (R ⇒ S) (T ⇒ U)
∪ⁿ-<:-∪ (R ∩ S) (T ⇒ U) = ∪ᶠ-<:-∪ (R ∩ S) (T ⇒ U)
∪ⁿ-<:-∪ (R ∪ S) (T ⇒ U) = <:-trans (<:-union (∪ⁿ-<:-∪ R (T ⇒ U)) <:-refl) (<:-∪-lub (<:-∪-lub (<:-trans <:-∪-left <:-∪-left) <:-∪-right) (<:-trans <:-∪-right <:-∪-left))
∪ⁿ-<:-∪ never (T ∩ U) = <:-∪-right
∪ⁿ-<:-∪ unknown (T ∩ U) = <:-∪-left
∪ⁿ-<:-∪ (R ⇒ S) (T ∩ U) = ∪ᶠ-<:-∪ (R ⇒ S) (T ∩ U)
∪ⁿ-<:-∪ (R ∩ S) (T ∩ U) = ∪ᶠ-<:-∪ (R ∩ S) (T ∩ U)
∪ⁿ-<:-∪ (R ∪ S) (T ∩ U) = <:-trans (<:-union (∪ⁿ-<:-∪ R (T ∩ U)) <:-refl) (<:-∪-lub (<:-∪-lub (<:-trans <:-∪-left <:-∪-left) <:-∪-right) (<:-trans <:-∪-right <:-∪-left))
∪ⁿ-<:-∪ S (T ∪ U) = <:-∪-lub (<:-trans (∪ⁿ-<:-∪ S T) (<:-union <:-refl <:-∪-left)) (<:-trans <:-∪-right <:-∪-right)
∪ⁿ-<:-∪ function function = <:-∪-left
∪ⁿ-<:-∪ function (T ⇒ U) = ∪ᶠ-<:-∪ function (T ⇒ U)
∪ⁿ-<:-∪ function (T ∩ U) = ∪ᶠ-<:-∪ function (T ∩ U)
∪ⁿ-<:-∪ (R ⇒ S) function = ∪ᶠ-<:-∪ (R ⇒ S) function
∪ⁿ-<:-∪ (R ∩ S) function = ∪ᶠ-<:-∪ (R ∩ S) function
∪ⁿ-<:-∪ (R ∪ S) function = <:-trans (<:-union (∪ⁿ-<:-∪ R function) <:-refl) (<:-∪-lub (<:-∪-lub (<:-trans <:-∪-left <:-∪-left) <:-∪-right) (<:-trans <:-∪-right <:-∪-left))
∪ⁿ-<:-∪ never function = <:-∪-right
∪ⁿ-<:-∪ unknown function = <:-∪-left

∪-<:-∪ⁿ S never = <:-∪-lub <:-refl <:-never
∪-<:-∪ⁿ S unknown = <:-unknown
∪-<:-∪ⁿ never (T ⇒ U) = <:-∪-lub <:-never <:-refl
∪-<:-∪ⁿ unknown (T ⇒ U) = <:-unknown
∪-<:-∪ⁿ (R ⇒ S) (T ⇒ U) = ∪-<:-∪ᶠ (R ⇒ S) (T ⇒ U)
∪-<:-∪ⁿ (R ∩ S) (T ⇒ U) = ∪-<:-∪ᶠ (R ∩ S) (T ⇒ U)
∪-<:-∪ⁿ (R ∪ S) (T ⇒ U) = <:-trans <:-∪-assocr (<:-trans (<:-union <:-refl <:-∪-symm) (<:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ R (T ⇒ U)) <:-refl)))
∪-<:-∪ⁿ never (T ∩ U) = <:-∪-lub <:-never <:-refl
∪-<:-∪ⁿ unknown (T ∩ U) = <:-unknown
∪-<:-∪ⁿ (R ⇒ S) (T ∩ U) = ∪-<:-∪ᶠ (R ⇒ S) (T ∩ U)
∪-<:-∪ⁿ (R ∩ S) (T ∩ U) = ∪-<:-∪ᶠ (R ∩ S) (T ∩ U)
∪-<:-∪ⁿ (R ∪ S) (T ∩ U) = <:-trans <:-∪-assocr (<:-trans (<:-union <:-refl <:-∪-symm) (<:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ R (T ∩ U)) <:-refl)))
∪-<:-∪ⁿ never (T ∪ U) = <:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ never T) <:-refl)
∪-<:-∪ⁿ unknown (T ∪ U) = <:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ unknown T) <:-refl)
∪-<:-∪ⁿ (R ⇒ S) (T ∪ U) = <:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ (R ⇒ S) T) <:-refl)
∪-<:-∪ⁿ (R ∩ S) (T ∪ U) = <:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ (R ∩ S) T) <:-refl)
∪-<:-∪ⁿ (R ∪ S) (T ∪ U) = <:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ (R ∪ S) T) <:-refl)
∪-<:-∪ⁿ function function = ∪-<:-∪ᶠ function function
∪-<:-∪ⁿ function (T ⇒ U) = ∪-<:-∪ᶠ function (T ⇒ U)
∪-<:-∪ⁿ function (T ∩ U) = ∪-<:-∪ᶠ function (T ∩ U)
∪-<:-∪ⁿ function (T ∪ U) = <:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ function T) <:-refl)
∪-<:-∪ⁿ (R ⇒ S) function = ∪-<:-∪ᶠ (R ⇒ S) function
∪-<:-∪ⁿ (R ∩ S) function = ∪-<:-∪ᶠ (R ∩ S) function
∪-<:-∪ⁿ (R ∪ S) function = <:-trans <:-∪-assocr (<:-trans (<:-union <:-refl <:-∪-symm) (<:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ R function) <:-refl)))
∪-<:-∪ⁿ never function = <:-∪-lub <:-never <:-refl
∪-<:-∪ⁿ unknown function = <:-unknown

⇒-<:-⇒ⁿ function T = <:-refl
⇒-<:-⇒ⁿ (R ⇒ S) T = <:-refl
⇒-<:-⇒ⁿ (R ∩ S) T = <:-refl
⇒-<:-⇒ⁿ (R ∪ S) T = <:-refl
⇒-<:-⇒ⁿ never T = <:-function-never
⇒-<:-⇒ⁿ unknown T = <:-refl

⇒ⁿ-<:-⇒ function T = <:-refl
⇒ⁿ-<:-⇒ (R ⇒ S) T = <:-refl
⇒ⁿ-<:-⇒ (R ∩ S) T = <:-refl
⇒ⁿ-<:-⇒ (R ∪ S) T = <:-refl
⇒ⁿ-<:-⇒ never T = <:-function-never
⇒ⁿ-<:-⇒ unknown T = <:-refl

normalize-<: : ∀ T → normalize T <: T
<:-normalize : ∀ T → T <: normalize T

<:-normalize nil = <:-∪-right
<:-normalize (S ⇒ T) = <:-trans (<:-function (normalize-<: S) (<:-normalize T)) (⇒-<:-⇒ⁿ (normal S) (normal T))
<:-normalize never = <:-refl
<:-normalize unknown = <:-refl
<:-normalize boolean = <:-∪-right
<:-normalize number = <:-∪-right
<:-normalize string = <:-∪-right
<:-normalize (S ∪ T) = <:-trans (<:-union (<:-normalize S) (<:-normalize T)) (∪-<:-∪ⁿ (normal S) (normal T))
<:-normalize (S ∩ T) = <:-trans (<:-intersect (<:-normalize S) (<:-normalize T)) (∩-<:-∩ⁿ (normal S) (normal T))

normalize-<: nil = <:-∪-lub <:-never <:-refl
normalize-<: (S ⇒ T) = <:-trans (⇒ⁿ-<:-⇒ (normal S) (normal T)) (<:-function (<:-normalize S) (normalize-<: T))
normalize-<: never = <:-refl
normalize-<: unknown = <:-refl
normalize-<: boolean = <:-∪-lub <:-never <:-refl
normalize-<: number = <:-∪-lub <:-never <:-refl
normalize-<: string = <:-∪-lub <:-never <:-refl
normalize-<: (S ∪ T) = <:-trans (∪ⁿ-<:-∪ (normal S) (normal T)) (<:-union (normalize-<: S) (normalize-<: T))
normalize-<: (S ∩ T) = <:-trans (∩ⁿ-<:-∩ (normal S) (normal T)) (<:-intersect (normalize-<: S) (normalize-<: T))


