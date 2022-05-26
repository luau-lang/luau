{-# OPTIONS --rewriting #-}

module Properties.ResolveOverloads where

open import FFI.Data.Either using (Left; Right)
open import Luau.Subtyping using (_<:_; _≮:_; Language; witness; scalar; unknown; never; function-ok)
open import Luau.Type using (Type ; _⇒_; unknown; never)
open import Luau.TypeSaturation using (saturate)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.DecSubtyping using (dec-subtyping; dec-subtypingⁿ; <:-impl-<:ᵒ)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; <:-trans-≮:; ≮:-trans-<:; <:-∩-left; <:-∩-right; <:-∩-glb; <:-∪-right; <:-impl-¬≮:; <:-unknown; <:-function; function-≮:-never; <:-never; unknown-≮:-function; scalar-≮:-function; ≮:-∪-right; scalar-≮:-never)
open import Properties.TypeNormalization using (Normal; FunType; normal; _⇒_; _∩_; _∪_; never; unknown; <:-normalize; normalize-<:; fun-≮:-never; unknown-≮:-fun; scalar-≮:-fun)
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

resolveᶠ : ∀ {F R} → FunType F → Normal R → Type
resolveᶠ Fᶠ Rⁿ = target (resolveˢ (normal-saturate Fᶠ) (saturated Fᶠ) Rⁿ (λ o → o))

resolveⁿ : ∀ {F R} → Normal F → Normal R → Type
resolveⁿ (Sⁿ ⇒ Tⁿ) Rⁿ = resolveᶠ (Sⁿ ⇒ Tⁿ) Rⁿ
resolveⁿ (Fᶠ ∩ Gᶠ) Rⁿ = resolveᶠ (Fᶠ ∩ Gᶠ) Rⁿ
resolveⁿ (Sⁿ ∪ Tˢ) Rⁿ = unknown
resolveⁿ unknown Rⁿ = unknown
resolveⁿ never Rⁿ = never

resolve : Type → Type → Type
resolve F R = resolveⁿ (normal F) (normal R)

<:-target-⇒ : ∀ {R S T} → (r : Resolved (S ⇒ T) R) → (T <: target r)
<:-target-⇒ (yes Sʳ Tʳ here x₁ x₂) = <:-refl
<:-target-⇒ (no x) = <:-unknown

<:-resolve-⇒ⁿ : ∀ {R S T} → (Fⁿ : Normal (S ⇒ T)) → (Rⁿ : Normal R) → T <: resolveⁿ Fⁿ Rⁿ
<:-resolve-⇒ⁿ (Sⁿ ⇒ Tⁿ) Rⁿ = <:-target-⇒ (resolveˢ (Sⁿ ⇒ Tⁿ) (saturated (Sⁿ ⇒ Tⁿ)) Rⁿ (λ o → o))

<:-resolve-⇒ : ∀ {R S T} → T <: resolve (S ⇒ T) R
<:-resolve-⇒ {R} {S} {T} = <:-trans (<:-normalize T) (<:-resolve-⇒ⁿ (normal (S ⇒ T)) (normal R))

resolveˢ-<:-⇒ : ∀ {F R U} → (FunType F) → (Saturated F) → (r : Resolved F R) → (F <: (R ⇒ U)) → (target r <: U)
resolveˢ-<:-⇒ Fᶠ Fˢ r F<:R⇒U with <:-impl-<:ᵒ Fᶠ Fˢ F<:R⇒U
resolveˢ-<:-⇒ Fᶠ Fˢ (yes Sʳ Tʳ oʳ R<:Sʳ tgtʳ) F<:R⇒U | defn o o₁ o₂ = <:-trans (tgtʳ o o₁) o₂
resolveˢ-<:-⇒ Fᶠ Fˢ (no tgtʳ) F<:R⇒U | defn o o₁ o₂ = CONTRADICTION (<:-impl-¬≮: o₁ (tgtʳ o))

resolveⁿ-<:-⇒ : ∀ {F R U} → (Fⁿ : Normal F) → (Rⁿ : Normal R) → (F <: (R ⇒ U)) → (resolveⁿ Fⁿ Rⁿ <: U)
resolveⁿ-<:-⇒ (Sⁿ ⇒ Tⁿ) Rⁿ F<:R⇒U = resolveˢ-<:-⇒ (normal-saturate (Sⁿ ⇒ Tⁿ)) (saturated (Sⁿ ⇒ Tⁿ)) (resolveˢ (normal-saturate (Sⁿ ⇒ Tⁿ)) (saturated (Sⁿ ⇒ Tⁿ)) Rⁿ (λ o → o)) F<:R⇒U
resolveⁿ-<:-⇒ (Fⁿ ∩ Gⁿ) Rⁿ F<:R⇒U = resolveˢ-<:-⇒ (normal-saturate (Fⁿ ∩ Gⁿ)) (saturated (Fⁿ ∩ Gⁿ)) (resolveˢ (normal-saturate (Fⁿ ∩ Gⁿ)) (saturated (Fⁿ ∩ Gⁿ)) Rⁿ (λ o → o)) (<:-trans (saturate-<: (Fⁿ ∩ Gⁿ)) F<:R⇒U)
resolveⁿ-<:-⇒ (Sⁿ ∪ Tˢ) Rⁿ F<:R⇒U = CONTRADICTION (<:-impl-¬≮: F<:R⇒U (<:-trans-≮: <:-∪-right (scalar-≮:-function Tˢ)))
resolveⁿ-<:-⇒ never Rⁿ F<:R⇒U = <:-never
resolveⁿ-<:-⇒ unknown Rⁿ F<:R⇒U = CONTRADICTION (<:-impl-¬≮: F<:R⇒U unknown-≮:-function)

resolve-<:-⇒ : ∀ {F R U} → (F <: (R ⇒ U)) → (resolve F R <: U)
resolve-<:-⇒ {F} {R} F<:R⇒U = resolveⁿ-<:-⇒ (normal F) (normal R) (<:-trans (normalize-<: F) (<:-trans F<:R⇒U (<:-function (normalize-<: R) <:-refl)))

resolve-≮:-⇒ : ∀ {F R U} → (resolve F R ≮: U) → (F ≮: (R ⇒ U))
resolve-≮:-⇒ {F} {R} {U} FR≮:U with dec-subtyping F (R ⇒ U)
resolve-≮:-⇒ {F} {R} {U} FR≮:U | Left F≮:R⇒U = F≮:R⇒U
resolve-≮:-⇒ {F} {R} {U} FR≮:U | Right F<:R⇒U = CONTRADICTION (<:-impl-¬≮: (resolve-<:-⇒ F<:R⇒U) FR≮:U)

⇒-<:-resolveⁿ : ∀ {F V U} → (Fⁿ : Normal F) → (Vⁿ : Normal V) → (resolveⁿ Fⁿ Vⁿ <: U) → (F <: (V ⇒ U))
⇒-<:-resolveⁿ (Sⁿ ⇒ Tⁿ) Vⁿ FV<:U = {!!}
⇒-<:-resolveⁿ (Fⁿ ∩ Gⁿ) Vⁿ FV<:U = {!!}
⇒-<:-resolveⁿ (Fⁿ ∪ Tˢ) Vⁿ FV<:U = {!FV<:U!}
⇒-<:-resolveⁿ never Vⁿ FV<:U = <:-never
⇒-<:-resolveⁿ unknown Vⁿ FV<:U = {!FV<:U!}

⇒-<:-resolve : ∀ {F V U} → (resolve F V <: U) → (F <: (V ⇒ U))
⇒-<:-resolve {F} {V} {U} FV<:U = {!!}

⇒-≮:-resolve : ∀ {F V U} → (F ≮: (V ⇒ U)) → (resolve F V ≮: U)
⇒-≮:-resolve F≮:V⇒U = {!!}

<:-resolveˢ : ∀ {F G V W} → (r : Resolved F V) → (s : Resolved G W) → (F <: G) → (V <: W) → target r <: target s
<:-resolveˢ = {!!}

<:-resolveᶠ : ∀ {F G V W} → (Fᶠ : FunType F) → (Gᶠ : FunType G) → (Vⁿ : Normal V) → (Wⁿ : Normal W) → (F <: G) → (V <: W) → resolveᶠ Fᶠ Vⁿ <: resolveᶠ Gᶠ Wⁿ
<:-resolveᶠ Fᶠ Gᶠ Vⁿ Wⁿ F<:G V<:W = <:-resolveˢ (resolveˢ (normal-saturate Fᶠ) (saturated Fᶠ) Vⁿ (λ o → o)) (resolveˢ (normal-saturate Gᶠ) (saturated Gᶠ) Wⁿ (λ o → o)) (<:-trans (saturate-<: Fᶠ) (<:-trans F<:G (<:-saturate Gᶠ))) V<:W

<:-resolveⁿ : ∀ {F G V W} → (Fⁿ : Normal F) → (Gⁿ : Normal G) → (Vⁿ : Normal V) → (Wⁿ : Normal W) → (F <: G) → (V <: W) → resolveⁿ Fⁿ Vⁿ <: resolveⁿ Gⁿ Wⁿ
<:-resolveⁿ (Rⁿ ⇒ Sⁿ) (Tⁿ ⇒ Uⁿ) Vⁿ Wⁿ F<:G V<:W = <:-resolveᶠ (Rⁿ ⇒ Sⁿ) (Tⁿ ⇒ Uⁿ) Vⁿ Wⁿ F<:G V<:W
<:-resolveⁿ (Rⁿ ⇒ Sⁿ) (Gⁿ ∩ Hⁿ) Vⁿ Wⁿ F<:G V<:W = <:-resolveᶠ (Rⁿ ⇒ Sⁿ) (Gⁿ ∩ Hⁿ) Vⁿ Wⁿ F<:G V<:W
<:-resolveⁿ (Eⁿ ∩ Fⁿ) (Tⁿ ⇒ Uⁿ) Vⁿ Wⁿ F<:G V<:W = <:-resolveᶠ (Eⁿ ∩ Fⁿ) (Tⁿ ⇒ Uⁿ) Vⁿ Wⁿ F<:G V<:W
<:-resolveⁿ (Eⁿ ∩ Fⁿ) (Gⁿ ∩ Hⁿ) Vⁿ Wⁿ F<:G V<:W = <:-resolveᶠ (Eⁿ ∩ Fⁿ) (Gⁿ ∩ Hⁿ) Vⁿ Wⁿ F<:G V<:W
<:-resolveⁿ (Fⁿ ∪ Sˢ) (Tⁿ ⇒ Uⁿ) Vⁿ Wⁿ F<:G V<:W = CONTRADICTION (<:-impl-¬≮: F<:G (≮:-∪-right (scalar-≮:-function Sˢ)))
<:-resolveⁿ unknown (Tⁿ ⇒ Uⁿ) Vⁿ Wⁿ F<:G V<:W = CONTRADICTION (<:-impl-¬≮: F<:G unknown-≮:-function)
<:-resolveⁿ (Fⁿ ∪ Sˢ) (Gⁿ ∩ Hⁿ) Vⁿ Wⁿ F<:G V<:W = CONTRADICTION (<:-impl-¬≮: F<:G (≮:-∪-right (scalar-≮:-fun (Gⁿ ∩ Hⁿ) Sˢ)))
<:-resolveⁿ unknown (Gⁿ ∩ Hⁿ) Vⁿ Wⁿ F<:G V<:W = CONTRADICTION (<:-impl-¬≮: F<:G (unknown-≮:-fun (Gⁿ ∩ Hⁿ)))
<:-resolveⁿ (Rⁿ ⇒ Sⁿ) never Vⁿ Wⁿ F<:G V<:W = CONTRADICTION (<:-impl-¬≮: F<:G (fun-≮:-never (Rⁿ ⇒ Sⁿ)))
<:-resolveⁿ (Eⁿ ∩ Fⁿ) never Vⁿ Wⁿ F<:G V<:W = CONTRADICTION (<:-impl-¬≮: F<:G (fun-≮:-never (Eⁿ ∩ Fⁿ)))
<:-resolveⁿ (Fⁿ ∪ Sˢ) never Vⁿ Wⁿ F<:G V<:W = CONTRADICTION (<:-impl-¬≮: F<:G (≮:-∪-right (scalar-≮:-never Sˢ)))
<:-resolveⁿ unknown never Vⁿ Wⁿ F<:G V<:W = F<:G
<:-resolveⁿ never Gⁿ Vⁿ Wⁿ F<:G V<:W = <:-never
<:-resolveⁿ Fⁿ (Gⁿ ∪ Uˢ) Vⁿ Wⁿ F<:G V<:W = <:-unknown
<:-resolveⁿ Fⁿ unknown Vⁿ Wⁿ F<:G V<:W = <:-unknown

<:-resolve : ∀ {F G V W} → (F <: G) → (V <: W) → resolve F V <: resolve G W
<:-resolve F<:G V<:W = {!!}

