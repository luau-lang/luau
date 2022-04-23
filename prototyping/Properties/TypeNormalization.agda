{-# OPTIONS --rewriting #-}

module Properties.TypeNormalization where

open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src)
open import Luau.TypeNormalization using (_∪ⁿ_; _∩ⁿ_; _∪ᶠ_; _∩ᶠ_; _∪ˢ_; _∩ˢ_; _⇒ᶠ_; normalize)
open import Luau.Subtyping using (_<:_)
open import Properties.Subtyping using (<:-trans; <:-refl; <:-unknown; <:-never; <:-∪-left; <:-∪-right; <:-∪-lub; <:-∩-left; <:-∩-right; <:-∩-glb; ∪-dist-∩-<:; <:-function; <:-function-∪-∩; <:-everything)

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

-- Normalization produces normal types
normal : ∀ T → Normal (normalize T)
normal-∪ⁿ : ∀ {S T} → Normal S → Normal T → Normal (S ∪ⁿ T)
normal-∩ⁿ : ∀ {S T} → Normal S → Normal T → Normal (S ∩ⁿ T)
normal-∪ᶠ : ∀ {F G} → FunType F → FunType G → FunType (F ∪ᶠ G)
normal-∩ᶠ : ∀ {F G} → FunType F → FunType G → FunType (F ∩ᶠ G)
normal-∪ˢ : ∀ {F G} → ¬FunType F → ¬FunType G → ¬FunType (F ∪ˢ G)
normal-∩ˢ : ∀ {F G} → ¬FunType F → ¬FunType G → ¬FunType (F ∩ˢ G)
normal-⇒ᶠ : ∀ {S T} → Normal S → Normal T → FunType (S ⇒ᶠ T)

normal nil = never ∪ scalar nil
normal (S ⇒ T) = {!!}
normal never = never
normal unknown = unknown
normal boolean = never ∪ scalar boolean
normal number = never ∪ scalar number
normal string = never ∪ scalar string
normal (S ∪ T) = normal-∪ⁿ (normal S) (normal T)
normal (S ∩ T) = normal-∩ⁿ (normal S) (normal T)

normal-∪ⁿ never never = never
normal-∪ⁿ never unknown = unknown
normal-∪ⁿ never (scalar number) = scalar number
normal-∪ⁿ never (scalar boolean) = scalar boolean
normal-∪ⁿ never (scalar string) = scalar string
normal-∪ⁿ never (scalar nil) = scalar nil
normal-∪ⁿ never (T ⇒ U) = T ⇒ U
normal-∪ⁿ never (T ∩ U) = T ∩ U
normal-∪ⁿ never (T ∪ U) = T ∪ U
normal-∪ⁿ unknown never = unknown
normal-∪ⁿ unknown unknown = unknown
normal-∪ⁿ unknown (scalar number) = unknown
normal-∪ⁿ unknown (scalar boolean) = unknown
normal-∪ⁿ unknown (scalar string) = unknown
normal-∪ⁿ unknown (scalar nil) = unknown
normal-∪ⁿ unknown (T ⇒ U) = unknown
normal-∪ⁿ unknown (T ∩ U) = unknown
normal-∪ⁿ unknown (T ∪ U) = unknown
normal-∪ⁿ (scalar number) never = scalar number
normal-∪ⁿ (scalar number) unknown = unknown
normal-∪ⁿ (scalar number) (scalar number) = scalar number ∪ scalar number
normal-∪ⁿ (scalar number) (scalar boolean) = scalar number ∪ scalar boolean
normal-∪ⁿ (scalar number) (scalar string) = scalar number ∪ scalar string
normal-∪ⁿ (scalar number) (scalar nil) = scalar number ∪ scalar nil
normal-∪ⁿ (scalar number) (T ⇒ U) = {!!}
normal-∪ⁿ (scalar number) (T ∩ U) = {!!}
normal-∪ⁿ (scalar number) (T ∪ U) = {!!}
normal-∪ⁿ (scalar boolean) T = {!!}
normal-∪ⁿ (scalar string) T = {!!}
normal-∪ⁿ (scalar nil) T = {!!}
normal-∪ⁿ (S ⇒ S₁) T = {!!}
normal-∪ⁿ (x ∩ x₁) T = {!!}
normal-∪ⁿ (S ∪ x) T = {!!}
-- normal-∪ⁿ never (¬fun (S₁ ∪ S₂)) = ¬fun (S₁ ∪ S₂)
-- normal-∪ⁿ never (fun (S ⇒ T)) = fun (S ⇒ T)
-- normal-∪ⁿ never (fun (F₁ ∩ F₂)) = fun (F₁ ∩ F₂)
-- normal-∪ⁿ never (fun function) = fun function
-- normal-∪ⁿ never (both F S) = both F S
-- normal-∪ⁿ never unknown = unknown
-- normal-∪ⁿ (¬fun (scalar s)) never = ¬fun (scalar s)
-- normal-∪ⁿ (¬fun (scalar s)) (¬fun (scalar t)) = {!!}
-- normal-∪ⁿ (¬fun (scalar s)) (¬fun (T ∪ U)) = {!!}
-- normal-∪ⁿ (¬fun (scalar s)) (fun G) = {!!}
-- normal-∪ⁿ (¬fun (scalar s)) (both G T) = {!!}
-- normal-∪ⁿ (¬fun (scalar s)) unknown = {!!}
-- normal-∪ⁿ (¬fun (S ∪ S₁)) T = {!!}
-- normal-∪ⁿ (fun F) T = {!!}
-- normal-∪ⁿ (both F S) T = {!!}
-- normal-∪ⁿ unknown T = {!!}
-- normal-∪ⁿ never T = {!!}

normal-∩ⁿ S T = {!!}

-- normal-∪ⁿ S never = S
-- normal-∪ⁿ (F ∪ S) unknown = unknown
-- normal-∪ⁿ never unknown = unknown
-- normal-∪ⁿ unknown unknown = unknown
-- normal-∪ⁿ never (G ∪ T) = G ∪ T
-- normal-∪ⁿ unknown (G ∪ T) = unknown
-- normal-∪ⁿ (F ∪ S) (G ∪ T) = (normal-∪ᶠ F G) ∪ (normal-∪ˢ S T)

-- normal-∩ⁿ S never = never
-- normal-∩ⁿ (F ∪ S) unknown = F ∪ S
-- normal-∩ⁿ never unknown = never
-- normal-∩ⁿ unknown unknown = unknown
-- normal-∩ⁿ never (G ∪ T) = never
-- normal-∩ⁿ unknown (G ∪ T) = G ∪ T
-- normal-∩ⁿ (F ∪ S) (G ∪ T) = (normal-∩ᶠ F G) ∪ (normal-∩ˢ S T)

normal-∪ᶠ (R ⇒ S) (T ⇒ U) = normal-⇒ᶠ (normal-∩ⁿ R T) (normal-∪ⁿ S U)
normal-∪ᶠ (R ⇒ S) (G₁ ∩ G₂) = normal-∩ᶠ (normal-∪ᶠ (R ⇒ S) G₁) (normal-∪ᶠ (R ⇒ S) G₂)
normal-∪ᶠ (F₁ ∩ F₂) (T ⇒ U) = normal-∩ᶠ (normal-∪ᶠ F₁ (T ⇒ U)) (normal-∪ᶠ F₂ (T ⇒ U))
normal-∪ᶠ (F₁ ∩ F₂) (G₁ ∩ G₂) = normal-∩ᶠ (normal-∪ᶠ F₁ (G₁ ∩ G₂)) (normal-∪ᶠ F₂ (G₁ ∩ G₂))

normal-∩ᶠ (F₁ ∩ F₂) (T ⇒ U) = (F₁ ∩ F₂) ∩ (T ⇒ U)
normal-∩ᶠ (F₁ ∩ F₂) (G ∩ G₁) = (F₁ ∩ F₂) ∩ (G ∩ G₁)
normal-∩ᶠ (R ⇒ S) (G₁ ∩ G₂) = (R ⇒ S) ∩ (G₁ ∩ G₂)
normal-∩ᶠ (R ⇒ S) (T ⇒ U) = (R ⇒ S) ∩ (T ⇒ U)

normal-∪ˢ F G = {!!}

normal-∩ˢ F G = {!!}

normal-⇒ᶠ S T = {!!}

-- record _<:forget_ {P : Type → Set} (S : Type) (T : ∃ P) : Set where
--   constructor ⟨_⟩
--   field sub : (S <: forget T)
-- open _<:forget_ public

-- record _:>forget_ {P : Type → Set} (S : Type) (T : ∃ P) : Set where
--   constructor ⟨_⟩
--   field sup : (forget T <: S)
-- open _:>forget_ public

-- forget-∩ⁿ-<: : ∀ {S T} → (forget S ∩ forget T) <:forget (S ∩ⁿ T)
-- forget-∩ⁿ-<: = {!!}

-- <:-forget-∪ⁿ : ∀ {S T} → (forget S ∪ forget T) :>forget (S ∪ⁿ T) 
-- <:-forget-∪ⁿ = {!!}

-- <:-forget-⇒ⁿ : ∀ {S T} →  (forget S ⇒ forget T) :>forget (S ⇒ⁿ T)
-- <:-forget-⇒ⁿ = {!!}

-- <:-forget-∩ᶠ : ∀ {F G} → (forget F ∩ forget G) :>forget (F ∩ᶠ G)
-- <:-forget-∩ᶠ = {!!}

-- <:-forget-∪ᶠ : ∀ {F G} → (forget F ∪ forget G) :>forget (F ∪ᶠ G) 
-- <:-forget-∪ᶠ {⟨ R ⇒ S ⟩} {⟨ T ⇒ U ⟩} = ⟨ <:-trans (sup <:-forget-⇒ⁿ) {!!} ⟩ -- <:-trans (<:-forget-⇒ⁿ {⟨ R ⟩ ∩ⁿ ⟨ T ⟩} {⟨ S ⟩ ∪ⁿ ⟨ U ⟩}) (<:-trans (<:-function (forget-∩ⁿ-<: { ⟨ R ⟩ }) (<:-forget-∪ⁿ {⟨ S ⟩})) <:-function-∪-∩)
-- <:-forget-∪ᶠ {⟨ R ⇒ S ⟩} {⟨ G₁ ∩ G₂ ⟩} = {!!} -- <:-trans (<:-trans (<:-forget-∩ᶠ {⟨ R ⇒ S ⟩ ∪ᶠ ⟨ G₁ ⟩}) {!!}) ∪-dist-∩-<:
-- <:-forget-∪ᶠ {⟨ R ⇒ S ⟩} {⟨ never ⟩} = {!!}
-- <:-forget-∪ᶠ {⟨ R ⇒ S ⟩} {⟨ function ⟩} = {!!}
-- <:-forget-∪ᶠ {⟨ F₁ ∩ F₂ ⟩} {⟨ G ⟩} = {!!}
-- <:-forget-∪ᶠ {⟨ never ⟩} {⟨ G ⟩} = ⟨ <:-∪-right ⟩
-- <:-forget-∪ᶠ {⟨ function ⟩} {⟨ G ⟩} = {!!}

-- forget-∪ᶠ-<: : ∀ {F G} → (forget F ∪ forget G) <: forget (F ∪ᶠ G)
-- forget-∪ᶠ-<: = {!!}

-- <:-∪ᶠ-left : ∀ {F G} → forget F <: forget (F ∪ᶠ G)
-- <:-∪ᶠ-left {F} = <:-trans (<:-∪-left {forget F}) (forget-∪ᶠ-<: {F})

-- -- <:-∪ᶠ-left {⟨ R ⇒ S ⟩} {⟨ T ⇒ U ⟩} = {!<:-∪-lub!}
-- -- <:-∪ᶠ-left {⟨ R ⇒ S ⟩} {⟨ G₁ ∩ G₂ ⟩} = {!<:-∩-glb!}
-- -- <:-∪ᶠ-left {⟨ R ⇒ S ⟩} {⟨ never ⟩} = <:-refl
-- -- <:-∪ᶠ-left {⟨ R ⇒ S ⟩} {⟨ function ⟩} = <:-function <:-never <:-unknown
-- -- <:-∪ᶠ-left {⟨ F₁ ∩ F₂ ⟩} {⟨ T ⇒ U ⟩} = {!!}
-- -- <:-∪ᶠ-left {⟨ F₁ ∩ F₂ ⟩} {⟨ G ∩ G₁ ⟩} = {!!}
-- -- <:-∪ᶠ-left {⟨ F₁ ∩ F₂ ⟩} {⟨ never ⟩} = <:-refl
-- -- <:-∪ᶠ-left {⟨ F₁ ∩ F₂ ⟩} {⟨ function ⟩} = {!<:-function <:-never <:-unknown!}
-- -- <:-∪ᶠ-left {⟨ never ⟩} {⟨ G ⟩} = <:-never
-- -- <:-∪ᶠ-left {⟨ function ⟩} {⟨ T ⇒ U ⟩} = <:-refl
-- -- <:-∪ᶠ-left {⟨ function ⟩} {⟨ G₁ ∩ G₂ ⟩} = <:-refl
-- -- <:-∪ᶠ-left {⟨ function ⟩} {⟨ never ⟩} = <:-refl
-- -- <:-∪ᶠ-left {⟨ function ⟩} {⟨ function ⟩} = <:-refl

-- <:-∪ⁿ-left : ∀ {S T} → forget S <: forget (S ∪ⁿ T)
-- <:-∪ⁿ-left {⟨ F ∪ S ⟩} {⟨ G ∪ T ⟩} = <:-∪-lub {!!} {!!}

-- <:-∪ⁿ-right : ∀ {S T} → forget T <: forget (S ∪ⁿ T)
-- <:-∪ⁿ-right = {!!}

-- <:-∪ⁿ-lub : ∀ {S T U} → forget S <: U → forget T <: U → forget (S ∪ⁿ T) <: U
-- <:-∪ⁿ-lub = {!!}

-- <:-∩ⁿ-left : ∀ {S T} → forget (S ∩ⁿ T) <: forget S
-- <:-∩ⁿ-left = {!!}

-- <:-∩ⁿ-right : ∀ {S T} → forget (S ∩ⁿ T) <: forget T
-- <:-∩ⁿ-right = {!!}

-- <:-∩ⁿ-glb : ∀ {S T U} → S <: forget T → S <: forget U → S <: forget (T ∩ⁿ U)
-- <:-∩ⁿ-glb = {!!}

-- normalize-<: : ∀ T → forget (normalize T) <: T
-- <:-normalize : ∀ T → T <: forget (normalize T)

-- <:-normalize nil = <:-∪-right
-- <:-normalize (S ⇒ T) = <:-trans (<:-function (normalize-<: S) (<:-normalize T)) <:-∪-left
-- <:-normalize never = <:-never
-- <:-normalize unknown = <:-everything
-- <:-normalize boolean = <:-∪-right
-- <:-normalize number = <:-∪-right
-- <:-normalize string = <:-∪-right
-- <:-normalize (S ∪ T) = <:-∪-lub (<:-trans (<:-normalize S) (<:-∪ⁿ-left {normalize S})) (<:-trans (<:-normalize T) (<:-∪ⁿ-right {normalize S}))
-- <:-normalize (S ∩ T) = <:-∩ⁿ-glb {S ∩ T} {normalize S} (<:-trans <:-∩-left (<:-normalize S)) (<:-trans <:-∩-right (<:-normalize T))

-- normalize-<: nil = <:-∪-lub <:-never <:-refl
-- normalize-<: (S ⇒ T) = <:-∪-lub (<:-function (<:-normalize S) (normalize-<: T)) <:-never
-- normalize-<: never = <:-∪-lub <:-never <:-refl
-- normalize-<: unknown = <:-unknown
-- normalize-<: boolean = <:-∪-lub <:-never <:-refl
-- normalize-<: number = <:-∪-lub <:-never <:-refl
-- normalize-<: string = <:-∪-lub <:-never <:-refl
-- normalize-<: (S ∪ T) = <:-∪ⁿ-lub {normalize S} (<:-trans (normalize-<: S) <:-∪-left) (<:-trans (normalize-<: T) <:-∪-right)
-- normalize-<: (S ∩ T) = <:-∩-glb (<:-trans (<:-∩ⁿ-left {normalize S}) (normalize-<: S)) (<:-trans (<:-∩ⁿ-right {normalize S}) (normalize-<: T))
