{-# OPTIONS --rewriting #-}

module Properties.ResolveOverloads where

open import FFI.Data.Either using (Left; Right)
open import Luau.Subtyping using (_<:_; _≮:_)
open import Luau.Type using (Type ; _⇒_; unknown; never)
open import Luau.TypeSaturation using (saturate)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.DecSubtyping using (dec-subtypingⁿ)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; <:-∩-left; <:-∩-right; <:-∩-glb; <:-impl-¬≮:; <:-unknown; function-≮:-never)
open import Properties.TypeNormalization using (Normal; FunType; normal; _⇒_; _∩_; _∪_; never; unknown; <:-normalize)
open import Properties.TypeSaturation using (Overloads; Saturated; _⊆ᵒ_; normal-saturate; saturated; defn; here; left; right)

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

  never :

    (F <: never) →
    --------------------------------------------
    ResolvedTo F G R

Resolved : Type → Type → Set
Resolved F R = ResolvedTo F F R

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
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F | _ | never q = never q
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Rⁿ G⊆F | never p | _ = never p

resolveᶠ : ∀ {F R} → FunType F → Normal R → Resolved (saturate F) R
resolveᶠ Fᶠ Rⁿ = resolveˢ (normal-saturate Fᶠ) (saturated Fᶠ) Rⁿ (λ o → o)

resolveⁿ : ∀ {F R} → Normal F → Normal R → Resolved (saturate F) R
resolveⁿ (Sⁿ ⇒ Tⁿ) Rⁿ = resolveᶠ (Sⁿ ⇒ Tⁿ) Rⁿ
resolveⁿ (Fᶠ ∩ Gᶠ) Rⁿ = resolveᶠ (Fᶠ ∩ Gᶠ) Rⁿ
resolveⁿ (Sⁿ ∪ Tˢ) Rⁿ = no (λ ())
resolveⁿ never Rⁿ = never <:-refl
resolveⁿ unknown Rⁿ = no (λ ())

resolve : Type → Type → Type
resolve F R with resolveⁿ (normal F) (normal R)
resolve F R | yes S T o R<:S p = T
resolve F R | no p = unknown
resolve F R | never p = never

<:-resolve : ∀ {R S T} → T <: resolve (S ⇒ T) R
<:-resolve {R} {S} {T} with resolveⁿ (normal (S ⇒ T)) (normal R)
<:-resolve {R} {S} {T} | yes Sⁿ Tⁿ here Rⁿ<:Sʳ p = <:-normalize T
<:-resolve {R} {S} {T} | no p = <:-unknown
<:-resolve {R} {S} {T} | never p = CONTRADICTION (<:-impl-¬≮: p function-≮:-never)
