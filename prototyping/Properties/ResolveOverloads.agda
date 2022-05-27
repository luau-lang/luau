{-# OPTIONS --rewriting #-}

module Properties.ResolveOverloads where

open import FFI.Data.Either using (Left; Right)
open import Luau.Subtyping using (_<:_; _≮:_; Language; witness; scalar; unknown; never; function-ok)
open import Luau.Type using (Type ; _⇒_; unknown; never)
open import Luau.TypeSaturation using (saturate)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.DecSubtyping using (dec-subtyping; dec-subtypingⁿ; <:-impl-<:ᵒ)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; <:-trans-≮:; ≮:-trans-<:; <:-∩-left; <:-∩-right; <:-∩-glb; <:-impl-¬≮:; <:-unknown; <:-function; function-≮:-never; <:-never; unknown-≮:-function; scalar-≮:-function; ≮:-∪-right; scalar-≮:-never; <:-∪-left; <:-∪-right)
open import Properties.TypeNormalization using (Normal; FunType; normal; _⇒_; _∩_; _∪_; never; unknown; <:-normalize; normalize-<:; fun-≮:-never; unknown-≮:-fun; scalar-≮:-fun)
open import Properties.TypeSaturation using (Overloads; Saturated; _⊆ᵒ_; _<:ᵒ_; normal-saturate; saturated; <:-saturate; saturate-<:; defn; here; left; right)

data ResolvedTo F G V : Set where

  yes : ∀ Sʳ Tʳ →

    Overloads F (Sʳ ⇒ Tʳ) →
    (V <: Sʳ) → 
    (∀ {S T} → Overloads G (S ⇒ T) → (V <: S) → (Tʳ <: T)) →
    --------------------------------------------
    ResolvedTo F G V

  no :

    (∀ {S T} → Overloads G (S ⇒ T) → (V ≮: S)) →
    --------------------------------------------
    ResolvedTo F G V

Resolved : Type → Type → Set
Resolved F V = ResolvedTo F F V

target : ∀ {F V} → Resolved F V → Type
target (yes _ T _ _ _) = T
target (no _) = unknown

resolveˢ : ∀ {F G V} → FunType G → Saturated F → Normal V → (G ⊆ᵒ F) → ResolvedTo F G V
resolveˢ (Sⁿ ⇒ Tⁿ) (defn sat-∩ sat-∪) Vⁿ G⊆F with dec-subtypingⁿ Vⁿ Sⁿ
resolveˢ (Sⁿ ⇒ Tⁿ) (defn sat-∩ sat-∪) Vⁿ G⊆F | Left V≮:S = no (λ { here → V≮:S })
resolveˢ (Sⁿ ⇒ Tⁿ) (defn sat-∩ sat-∪) Vⁿ G⊆F | Right V<:S = yes _ _ (G⊆F here) V<:S (λ { here _ → <:-refl })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Vⁿ G⊆F with resolveˢ Gᶠ (defn sat-∩ sat-∪) Vⁿ (G⊆F ∘ left) | resolveˢ Hᶠ (defn sat-∩ sat-∪) Vⁿ (G⊆F ∘ right)
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Vⁿ G⊆F | yes S₁ T₁ o₁ V<:S₁ tgt₁ | yes S₂ T₂ o₂ V<:S₂ tgt₂ with sat-∩ o₁ o₂
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Vⁿ G⊆F | yes S₁ T₁ o₁ V<:S₁ tgt₁ | yes S₂ T₂ o₂ V<:S₂ tgt₂ | defn o p₁ p₂ =
  yes _ _ o (<:-trans (<:-∩-glb V<:S₁ V<:S₂) p₁) (λ { (left o) p → <:-trans p₂ (<:-trans <:-∩-left (tgt₁ o p)) ; (right o) p → <:-trans p₂ (<:-trans <:-∩-right (tgt₂ o p)) })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Vⁿ G⊆F | yes S₁ T₁ o₁ V<:S₁ tgt₁ | no src₂ =
  yes _ _ o₁ V<:S₁ (λ { (left o) p → tgt₁ o p ; (right o) p → CONTRADICTION (<:-impl-¬≮: p (src₂ o)) })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Vⁿ G⊆F | no src₁ | yes S₂ T₂ o₂ V<:S₂ tgt₂ =
  yes _ _ o₂ V<:S₂ (λ { (left o) p → CONTRADICTION (<:-impl-¬≮: p (src₁ o)) ; (right o) p → tgt₂ o p })
resolveˢ (Gᶠ ∩ Hᶠ) (defn sat-∩ sat-∪) Vⁿ G⊆F | no src₁ | no src₂ =
  no (λ { (left o) → src₁ o ; (right o) → src₂ o })

resolveᶠ : ∀ {F V} → FunType F → Normal V → Type
resolveᶠ Fᶠ Vⁿ = target (resolveˢ (normal-saturate Fᶠ) (saturated Fᶠ) Vⁿ (λ o → o))

resolveⁿ : ∀ {F V} → Normal F → Normal V → Type
resolveⁿ (Sⁿ ⇒ Tⁿ) Vⁿ = resolveᶠ (Sⁿ ⇒ Tⁿ) Vⁿ
resolveⁿ (Fᶠ ∩ Gᶠ) Vⁿ = resolveᶠ (Fᶠ ∩ Gᶠ) Vⁿ
resolveⁿ (Sⁿ ∪ Tˢ) Vⁿ = unknown
resolveⁿ unknown Vⁿ = unknown
resolveⁿ never Vⁿ = never

resolve : Type → Type → Type
resolve F V = resolveⁿ (normal F) (normal V)

resolveˢ-<:-⇒ : ∀ {F V U} → (FunType F) → (Saturated F) → (FunType (V ⇒ U)) → (r : Resolved F V) → (F <: (V ⇒ U)) → (target r <: U)
resolveˢ-<:-⇒ Fᶠ Fˢ V⇒Uᶠ r F<:V⇒U with <:-impl-<:ᵒ Fᶠ Fˢ V⇒Uᶠ F<:V⇒U here
resolveˢ-<:-⇒ Fᶠ Fˢ V⇒Uᶠ (yes Sʳ Tʳ oʳ V<:Sʳ tgtʳ) F<:V⇒U | defn o o₁ o₂ = <:-trans (tgtʳ o o₁) o₂
resolveˢ-<:-⇒ Fᶠ Fˢ V⇒Uᶠ (no tgtʳ) F<:V⇒U | defn o o₁ o₂ = CONTRADICTION (<:-impl-¬≮: o₁ (tgtʳ o))

resolveⁿ-<:-⇒ : ∀ {F V U} → (Fⁿ : Normal F) → (Vⁿ : Normal V) → (Uⁿ : Normal U) → (F <: (V ⇒ U)) → (resolveⁿ Fⁿ Vⁿ <: U)
resolveⁿ-<:-⇒ (Sⁿ ⇒ Tⁿ) Vⁿ Uⁿ F<:V⇒U = resolveˢ-<:-⇒ (normal-saturate (Sⁿ ⇒ Tⁿ)) (saturated (Sⁿ ⇒ Tⁿ)) (Vⁿ ⇒ Uⁿ) (resolveˢ (normal-saturate (Sⁿ ⇒ Tⁿ)) (saturated (Sⁿ ⇒ Tⁿ)) Vⁿ (λ o → o)) F<:V⇒U
resolveⁿ-<:-⇒ (Fⁿ ∩ Gⁿ) Vⁿ Uⁿ F<:V⇒U = resolveˢ-<:-⇒ (normal-saturate (Fⁿ ∩ Gⁿ)) (saturated (Fⁿ ∩ Gⁿ)) (Vⁿ ⇒ Uⁿ) (resolveˢ (normal-saturate (Fⁿ ∩ Gⁿ)) (saturated (Fⁿ ∩ Gⁿ)) Vⁿ (λ o → o)) (<:-trans (saturate-<: (Fⁿ ∩ Gⁿ)) F<:V⇒U)
resolveⁿ-<:-⇒ (Sⁿ ∪ Tˢ) Vⁿ Uⁿ F<:V⇒U = CONTRADICTION (<:-impl-¬≮: F<:V⇒U (<:-trans-≮: <:-∪-right (scalar-≮:-function Tˢ)))
resolveⁿ-<:-⇒ never Vⁿ Uⁿ F<:V⇒U = <:-never
resolveⁿ-<:-⇒ unknown Vⁿ Uⁿ F<:V⇒U = CONTRADICTION (<:-impl-¬≮: F<:V⇒U unknown-≮:-function)

resolve-<:-⇒ : ∀ {F V U} → (F <: (V ⇒ U)) → (resolve F V <: U)
resolve-<:-⇒ {F} {V} {U} F<:V⇒U = <:-trans (resolveⁿ-<:-⇒ (normal F) (normal V) (normal U) (<:-trans (normalize-<: F) (<:-trans F<:V⇒U (<:-normalize (V ⇒ U))))) (normalize-<: U)

resolve-≮:-⇒ : ∀ {F V U} → (resolve F V ≮: U) → (F ≮: (V ⇒ U))
resolve-≮:-⇒ {F} {V} {U} FV≮:U with dec-subtyping F (V ⇒ U)
resolve-≮:-⇒ {F} {V} {U} FV≮:U | Left F≮:V⇒U = F≮:V⇒U
resolve-≮:-⇒ {F} {V} {U} FV≮:U | Right F<:V⇒U = CONTRADICTION (<:-impl-¬≮: (resolve-<:-⇒ F<:V⇒U) FV≮:U)

<:-resolveˢ-⇒ : ∀ {S T V} → (r : Resolved (S ⇒ T) V) → (V <: S) → T <: target r
<:-resolveˢ-⇒ (yes S T here _ _) V<:S = <:-refl
<:-resolveˢ-⇒ (no _) V<:S = <:-unknown

<:-resolveⁿ-⇒ : ∀ {S T V} → (Sⁿ : Normal S) → (Tⁿ : Normal T) → (Vⁿ : Normal V) → (V <: S) → T <: resolveⁿ (Sⁿ ⇒ Tⁿ) Vⁿ
<:-resolveⁿ-⇒ Sⁿ Tⁿ Vⁿ V<:S = <:-resolveˢ-⇒ (resolveˢ (Sⁿ ⇒ Tⁿ) (saturated (Sⁿ ⇒ Tⁿ)) Vⁿ (λ o → o)) V<:S 

<:-resolve-⇒ : ∀ {S T V} → (V <: S) → T <: resolve (S ⇒ T) V
<:-resolve-⇒ {S} {T} {V} V<:S = <:-trans (<:-normalize T) (<:-resolveⁿ-⇒ (normal S) (normal T) (normal V) (<:-trans (normalize-<: V) (<:-trans V<:S (<:-normalize S))))

<:-resolveˢ : ∀ {F G V W} → (r : Resolved F V) → (s : Resolved G W) → (F <:ᵒ G) → (V <: W) → target r <: target s
<:-resolveˢ (yes Sʳ Tʳ oʳ V<:Sʳ tgtʳ) (yes Sˢ Tˢ oˢ  W<:Sˢ tgtˢ) F<:G V<:W with F<:G oˢ
<:-resolveˢ (yes Sʳ Tʳ oʳ V<:Sʳ tgtʳ) (yes Sˢ Tˢ oˢ W<:Sˢ tgtˢ) F<:G V<:W | defn o o₁ o₂ = <:-trans (tgtʳ o (<:-trans (<:-trans V<:W W<:Sˢ) o₁)) o₂
<:-resolveˢ (no r) (yes Sˢ Tˢ oˢ  W<:Sˢ tgtˢ) F<:G V<:W with F<:G oˢ
<:-resolveˢ (no r) (yes Sˢ Tˢ oˢ  W<:Sˢ tgtˢ) F<:G V<:W | defn o o₁ o₂ = CONTRADICTION (<:-impl-¬≮: (<:-trans V<:W (<:-trans W<:Sˢ o₁)) (r o))
<:-resolveˢ r (no s) F<:G V<:W = <:-unknown

<:-resolveᶠ : ∀ {F G V W} → (Fᶠ : FunType F) → (Gᶠ : FunType G) → (Vⁿ : Normal V) → (Wⁿ : Normal W) → (F <: G) → (V <: W) → resolveᶠ Fᶠ Vⁿ <: resolveᶠ Gᶠ Wⁿ
<:-resolveᶠ Fᶠ Gᶠ Vⁿ Wⁿ F<:G V<:W = <:-resolveˢ
  (resolveˢ (normal-saturate Fᶠ) (saturated Fᶠ) Vⁿ (λ o → o))
  (resolveˢ (normal-saturate Gᶠ) (saturated Gᶠ) Wⁿ (λ o → o))
  (<:-impl-<:ᵒ (normal-saturate Fᶠ) (saturated Fᶠ) (normal-saturate Gᶠ) (<:-trans (saturate-<: Fᶠ) (<:-trans F<:G (<:-saturate Gᶠ))))
  V<:W

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
<:-resolve {F} {G} {V} {W} F<:G V<:W = <:-resolveⁿ (normal F) (normal G) (normal V) (normal W)
  (<:-trans (normalize-<: F) (<:-trans F<:G (<:-normalize G)))
  (<:-trans (normalize-<: V) (<:-trans V<:W (<:-normalize W)))
