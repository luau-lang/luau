{-# OPTIONS --rewriting #-}

module Properties.ResolveOverloads where

open import FFI.Data.Either using (Left; Right)
open import Luau.Subtyping using (_<:_; _≮:_; Language; witness; scalar; unknown; never; function-ok)
open import Luau.Type using (Type ; _⇒_; unknown; never)
open import Luau.TypeSaturation using (saturate)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.DecSubtyping using (dec-subtyping; dec-subtypingⁿ; <:-impl-<:ᵒ)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; <:-trans-≮:; ≮:-trans-<:; <:-∩-left; <:-∩-right; <:-∩-glb; <:-∪-right; <:-impl-¬≮:; <:-unknown; <:-function; function-≮:-never; <:-never; unknown-≮:-function; scalar-≮:-function)
open import Properties.TypeNormalization using (Normal; FunType; normal; _⇒_; _∩_; _∪_; never; unknown; <:-normalize; normalize-<:)
open import Properties.TypeSaturation using (Overloads; Saturated; _⊆ᵒ_; _<:ᵒ_; normal-saturate; saturated; <:-saturate; saturate-<:; defn; here; left; right)

data ResolvedTo F G R : Set where

  yes : ∀ Sʳ Tʳ →

    Overloads F (Sʳ ⇒ Tʳ) →
    (R <: Sʳ) → 
    (∀ {S T} → Overloads G (S ⇒ T) → (R <: S) → (Tʳ <: T)) →
    --------------------------------------------
    ResolvedTo F G R

  no :

    (∀ {S T} → Overloads G (S ⇒ T) → (R ≮: S)) →
    --------------------------------------------
    ResolvedTo F G R

Resolved : Type → Type → Set
Resolved F R = ResolvedTo F F R

target : ∀ {F R} → Resolved F R → Type
target (yes _ T _ _ _) = T
target (no _) = unknown

resolveˢ : ∀ {F G R} → FunType G → Saturated F → Normal R → (G ⊆ᵒ F) → ResolvedTo F G R
resolveˢ (Sⁿ ⇒ Tⁿ) (defn sat-∩ sat-∪) Rⁿ G⊆F with dec-subtypingⁿ Rⁿ Sⁿ
resolveˢ (Sⁿ ⇒ Tⁿ) (defn sat-∩ sat-∪) Rⁿ G⊆F | Left R≮:S = no (λ { here → R≮:S })
resolveˢ (Sⁿ ⇒ Tⁿ) (defn sat-∩ sat-∪) Rⁿ G⊆F | Right R<:S = yes _ _ (G⊆F here) R<:S (λ { here _ → <:-refl })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F with resolveˢ Gᶠ (defn sat-∩ sat-∪) Rⁿ (G⊆F ∘ left) | resolveˢ Hᶠ (defn sat-∩ sat-∪) Rⁿ (G⊆F ∘ right)
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F | yes S₁ T₁ o₁ R<:S₁ tgt₁ | yes S₂ T₂ o₂ R<:S₂ tgt₂ with sat-∩ o₁ o₂
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F | yes S₁ T₁ o₁ R<:S₁ tgt₁ | yes S₂ T₂ o₂ R<:S₂ tgt₂ | defn o p₁ p₂ =
  yes _ _ o (<:-trans (<:-∩-glb R<:S₁ R<:S₂) p₁) (λ { (left o) p → <:-trans p₂ (<:-trans <:-∩-left (tgt₁ o p)) ; (right o) p → <:-trans p₂ (<:-trans <:-∩-right (tgt₂ o p)) })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F | yes S₁ T₁ o₁ R<:S₁ tgt₁ | no src₂ =
  yes _ _ o₁ R<:S₁ (λ { (left o) p → tgt₁ o p ; (right o) p → CONTRADICTION (<:-impl-¬≮: p (src₂ o)) })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F | no src₁ | yes S₂ T₂ o₂ R<:S₂ tgt₂ =
  yes _ _ o₂ R<:S₂ (λ { (left o) p → CONTRADICTION (<:-impl-¬≮: p (src₁ o)) ; (right o) p → tgt₂ o p })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F | no src₁ | no src₂ =
  no (λ { (left o) → src₁ o ; (right o) → src₂ o })

resolveᶠ : ∀ {F R} → FunType F → Normal R → Resolved (saturate F) R
resolveᶠ Fᶠ Rⁿ = resolveˢ (normal-saturate Fᶠ) (saturated Fᶠ) Rⁿ (λ o → o)

resolveⁿ : ∀ {F R} → Normal F → Normal R → Type
resolveⁿ (Sⁿ ⇒ Tⁿ) Rⁿ = target (resolveᶠ (Sⁿ ⇒ Tⁿ) Rⁿ)
resolveⁿ (Fᶠ ∩ Gᶠ) Rⁿ = target (resolveᶠ (Fᶠ ∩ Gᶠ) Rⁿ)
resolveⁿ (Sⁿ ∪ Tˢ) Rⁿ = unknown
resolveⁿ unknown Rⁿ = unknown
resolveⁿ never Rⁿ = never

resolve : Type → Type → Type
resolve F R = resolveⁿ (normal F) (normal R)

<:-target-⇒ : ∀ {R S T} → (r : Resolved (S ⇒ T) R) → (T <: target r)
<:-target-⇒ (yes Sʳ Tʳ here x₁ x₂) = <:-refl
<:-target-⇒ (no x) = <:-unknown

<:-resolveⁿ : ∀ {R S T} → (Fⁿ : Normal (S ⇒ T)) → (Rⁿ : Normal R) → T <: resolveⁿ Fⁿ Rⁿ
<:-resolveⁿ (Sⁿ ⇒ Tⁿ) Rⁿ = <:-target-⇒ (resolveˢ (Sⁿ ⇒ Tⁿ) (saturated (Sⁿ ⇒ Tⁿ)) Rⁿ (λ o → o))

<:-resolve : ∀ {R S T} → T <: resolve (S ⇒ T) R
<:-resolve {R} {S} {T} = <:-trans (<:-normalize T) (<:-resolveⁿ (normal (S ⇒ T)) (normal R))

resolveˢ-<:-⇒ : ∀ {F R U} → (FunType F) → (Saturated F) → (r : Resolved F R) → (R ≮: never) → (F <: (R ⇒ U)) → (target r <: U)
resolveˢ-<:-⇒ Fᶠ Fˢ r R≮:never F<:R⇒U with <:-impl-<:ᵒ Fᶠ Fˢ R≮:never F<:R⇒U
resolveˢ-<:-⇒ Fᶠ Fˢ (yes Sʳ Tʳ oʳ R<:Sʳ tgtʳ) R≮:never F<:R⇒U | defn o o₁ o₂ = <:-trans (tgtʳ o o₁) o₂
resolveˢ-<:-⇒ Fᶠ Fˢ (no tgtʳ) R≮:never F<:R⇒U | defn o o₁ o₂ = CONTRADICTION (<:-impl-¬≮: o₁ (tgtʳ o))

resolveⁿ-<:-⇒ : ∀ {F R U} → (Fⁿ : Normal F) → (Rⁿ : Normal R) → (R ≮: never) → (F <: (R ⇒ U)) → (resolveⁿ Fⁿ Rⁿ <: U)
resolveⁿ-<:-⇒ (Sⁿ ⇒ Tⁿ) Rⁿ R≮:never F<:R⇒U = resolveˢ-<:-⇒ (normal-saturate (Sⁿ ⇒ Tⁿ)) (saturated (Sⁿ ⇒ Tⁿ)) (resolveˢ (normal-saturate (Sⁿ ⇒ Tⁿ)) (saturated (Sⁿ ⇒ Tⁿ)) Rⁿ (λ o → o)) R≮:never F<:R⇒U
resolveⁿ-<:-⇒ (Fⁿ ∩ Gⁿ) Rⁿ R≮:never F<:R⇒U = resolveˢ-<:-⇒ (normal-saturate (Fⁿ ∩ Gⁿ)) (saturated (Fⁿ ∩ Gⁿ)) (resolveˢ (normal-saturate (Fⁿ ∩ Gⁿ)) (saturated (Fⁿ ∩ Gⁿ)) Rⁿ (λ o → o)) R≮:never (<:-trans (saturate-<: (Fⁿ ∩ Gⁿ)) F<:R⇒U)
resolveⁿ-<:-⇒ (Sⁿ ∪ Tˢ) Rⁿ R≮:never F<:R⇒U = CONTRADICTION (<:-impl-¬≮: F<:R⇒U (<:-trans-≮: <:-∪-right (scalar-≮:-function Tˢ)))
resolveⁿ-<:-⇒ never Rⁿ R≮:never F<:R⇒U = <:-never
resolveⁿ-<:-⇒ unknown Rⁿ R≮:never F<:R⇒U = CONTRADICTION (<:-impl-¬≮: F<:R⇒U unknown-≮:-function)

resolve-<:-⇒ : ∀ {F R U} → (R ≮: never) → (F <: (R ⇒ U)) → (resolve F R <: U)
resolve-<:-⇒ {F} {R} R≮:never F<:R⇒U = resolveⁿ-<:-⇒ (normal F) (normal R) (<:-trans-≮: (<:-normalize R) R≮:never) (<:-trans (normalize-<: F) (<:-trans F<:R⇒U (<:-function (normalize-<: R) <:-refl)))

resolve-≮:-⇒ : ∀ {F R U} → (R ≮: never) → (resolve F R ≮: U) → (F ≮: (R ⇒ U))
resolve-≮:-⇒ {F} {R} {U} R≮:never FR≮:U with dec-subtyping F (R ⇒ U)
resolve-≮:-⇒ {F} {R} {U} R≮:never FR≮:U | Left F≮:R⇒U = F≮:R⇒U
resolve-≮:-⇒ {F} {R} {U} R≮:never FR≮:U | Right F<:R⇒U = CONTRADICTION (<:-impl-¬≮: (resolve-<:-⇒ R≮:never F<:R⇒U) FR≮:U)
