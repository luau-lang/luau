{-# OPTIONS --rewriting #-}

module Properties.TypeNormalization where

open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src)
open import Luau.TypeNormalization using (_∪ⁿ_; _∩ⁿ_; _∪ᶠ_; _∩ᶠ_; _∪ⁿˢ_; _∩ⁿˢ_; _⇒ᶠ_; normalize)
open import Luau.Subtyping using (_<:_)
open import Properties.Subtyping using (<:-trans; <:-refl; <:-unknown; <:-never; <:-∪-left; <:-∪-right; <:-∪-lub; <:-∩-left; <:-∩-right; <:-∩-glb; ∪-dist-∩-<:; <:-function; <:-function-∪-∩; <:-everything; <:-union; <:-∪-assocl; <:-∪-assocr; <:-∪-symm)

-- Notmal forms for types
data FunType : Type → Set
data Normal : Type → Set

data FunType where
  _⇒_ : ∀ {S T} → Normal S → Normal T → FunType (S ⇒ T)
  _∩_ : ∀ {F G} → FunType F → FunType G → FunType (F ∩ G)

data Normal where 
  never : Normal never
  unknown : Normal unknown
  _⇒_ : ∀ {S T} → Normal S → Normal T → Normal (S ⇒ T)
  _∩_ : ∀ {F G} → FunType F → FunType G → Normal (F ∩ G)
  _∪_ : ∀ {S T} → Normal S → Scalar T → Normal (S ∪ T)

data OptScalar : Type → Set where
  never : OptScalar never
  number : OptScalar number
  boolean : OptScalar boolean
  string : OptScalar string
  nil : OptScalar nil
  
-- Normalization produces normal types
normal : ∀ T → Normal (normalize T)
normalᶠ : ∀ {F} → FunType F → Normal F
normal-∪ⁿ : ∀ {S T} → Normal S → Normal T → Normal (S ∪ⁿ T)
normal-∩ⁿ : ∀ {S T} → Normal S → Normal T → Normal (S ∩ⁿ T)
normal-∪ⁿˢ : ∀ {S T} → Normal S → OptScalar T → Normal (S ∪ⁿˢ T)
normal-∩ⁿˢ : ∀ {S T} → Normal S → Scalar T → OptScalar (S ∩ⁿˢ T)
normal-∪ᶠ : ∀ {F G} → FunType F → FunType G → FunType (F ∪ᶠ G)
normal-∩ᶠ : ∀ {F G} → FunType F → FunType G → FunType (F ∩ᶠ G)
normal-⇒ᶠ : ∀ {S T} → Normal S → Normal T → FunType (S ⇒ᶠ T)

normal nil = never ∪ nil
normal (S ⇒ T) = normalᶠ (normal-⇒ᶠ (normal S) (normal T))
normal never = never
normal unknown = unknown
normal boolean = never ∪ boolean
normal number = never ∪ number
normal string = never ∪ string
normal (S ∪ T) = normal-∪ⁿ (normal S) (normal T)
normal (S ∩ T) = normal-∩ⁿ (normal S) (normal T)

normalᶠ (S ⇒ T) = S ⇒ T
normalᶠ (F ∩ G) = F ∩ G

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

normal-∩ⁿˢ never number = never
normal-∩ⁿˢ never boolean = never
normal-∩ⁿˢ never string = never
normal-∩ⁿˢ never nil = never
normal-∩ⁿˢ unknown number = never
normal-∩ⁿˢ unknown boolean = never
normal-∩ⁿˢ unknown string = never
normal-∩ⁿˢ unknown nil = never
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

normal-⇒ᶠ never T = never ⇒ unknown
normal-⇒ᶠ unknown T = unknown ⇒ T
normal-⇒ᶠ (R ⇒ S) T = (R ⇒ S) ⇒ T
normal-⇒ᶠ (R ∩ S) T = (R ∩ S) ⇒ T
normal-⇒ᶠ (R ∪ S) T = (R ∪ S) ⇒ T

normal-∩ᶠ F G = F ∩ G

normal-∪ᶠ (R ⇒ S) (T ⇒ U) = normal-⇒ᶠ (normal-∩ⁿ R T) (normal-∪ⁿ S U)
normal-∪ᶠ (R ⇒ S) (G ∩ H) = normal-∪ᶠ (R ⇒ S) G ∩ normal-∪ᶠ (R ⇒ S) H
normal-∪ᶠ (E ∩ F) G = normal-∪ᶠ E G ∩ normal-∪ᶠ F G

∪ᶠ-<:-∪ : ∀ {F G} → FunType F → FunType G → (F ∪ᶠ G) <: (F ∪ G)
∪ᶠ-<:-∪ F G = {!!}

∪-<:-∪ᶠ : ∀ {F G} → FunType F → FunType G → (F ∪ G) <: (F ∪ᶠ G)
∪-<:-∪ᶠ F G = {!!}

∪ⁿ-<:-∪ : ∀ {S T} → Normal S → Normal T → (S ∪ⁿ T) <: (S ∪ T)
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

∪-<:-∪ⁿ : ∀ {S T} → Normal S → Normal T → (S ∪ T) <: (S ∪ⁿ T)
∪-<:-∪ⁿ S never = <:-∪-lub <:-refl <:-never
∪-<:-∪ⁿ S unknown = <:-unknown
∪-<:-∪ⁿ never (T ⇒ U) = <:-∪-lub <:-never <:-refl
∪-<:-∪ⁿ unknown (T ⇒ U) = <:-unknown
∪-<:-∪ⁿ (R ⇒ S) (T ⇒ U) = ∪-<:-∪ᶠ (R ⇒ S) (T ⇒ U)
∪-<:-∪ⁿ (R ∩ S) (T ⇒ U) = ∪-<:-∪ᶠ (R ∩ S) (T ⇒ U)
∪-<:-∪ⁿ (R ∪ S) (T ⇒ U) = {! <:-trans <:-∪-assocr (<:-trans (<:-union <:-refl <:-∪-symm) (<:-trans <:-∪-assocl (<:-union (∪-<:-∪ⁿ R (T ⇒ U)) <:-refl)))!}
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

normalize-<: : ∀ T → normalize T <: T
<:-normalize : ∀ T → T <: normalize T

<:-normalize nil = <:-∪-right
<:-normalize (S ⇒ T) = {!!}
<:-normalize never = <:-refl
<:-normalize unknown = <:-refl
<:-normalize boolean = <:-∪-right
<:-normalize number = <:-∪-right
<:-normalize string = <:-∪-right
<:-normalize (S ∪ T) = <:-trans (<:-union (<:-normalize S) (<:-normalize T)) (∪-<:-∪ⁿ (normal S) (normal T))
<:-normalize (S ∩ T) = {!!}

normalize-<: nil = <:-∪-lub <:-never <:-refl
normalize-<: (S ⇒ T) = {!!}
normalize-<: never = <:-refl
normalize-<: unknown = <:-refl
normalize-<: boolean = <:-∪-lub <:-never <:-refl
normalize-<: number = <:-∪-lub <:-never <:-refl
normalize-<: string = <:-∪-lub <:-never <:-refl
normalize-<: (S ∪ T) = <:-trans (∪ⁿ-<:-∪ (normal S) (normal T)) (<:-union (normalize-<: S) (normalize-<: T))
normalize-<: (S ∩ T) = {!!}


