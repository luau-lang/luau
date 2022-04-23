{-# OPTIONS --rewriting #-}

module Properties.OverloadedFunctions where

open import FFI.Data.Either using (Either; Left; Right)
open import Luau.OverloadedFunctions using (resolve)
open import Luau.Subtyping using (_<:_; _≮:_; witness; _,_; left; right)
open import Luau.Type using (Type; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.DecSubtyping using (dec-subtyping)
open import Properties.Subtyping using (<:-refl; <:-trans; <:-union; <:-∪-left; <:-∪-right; <:-∪-lub; <:-intersect; <:-∩-left; <:-∩-right; <:-∩-glb; <:-impl-¬≮:; <:-function; <:-never; <:-never-left; <:-never-right; <:-function-∩; <:-function-∩-∪; <:-∩-dist-∪; <:-trans-≮:; never-≮:; ≮:-∩-left; ≮:-∩-right; ≮:-∪-left; ≮:-∪-right)

-- Function overload resolution respects subtyping

resolve-<: : ∀ F {S T} → (S <: T) → (resolve F S <: resolve F T)
resolve-<: nil p = <:-refl
resolve-<: (F ⇒ F₁) p = <:-refl
resolve-<: never p = <:-refl
resolve-<: unknown p = <:-refl
resolve-<: boolean p = <:-refl
resolve-<: number p = <:-refl
resolve-<: string p = <:-refl
resolve-<: (F ∪ G) p = <:-union (resolve-<: F p) (resolve-<: G p)
resolve-<: (F ∩ G) {S} {T} p with dec-subtyping S (src F) | dec-subtyping S (src G) | dec-subtyping T (src F) | dec-subtyping T (src G)
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | Left q₂  | Left q₃  | Left q₄  = <:-union (resolve-<: F p) (resolve-<: G p)
resolve-<: (F ∩ G) {S} {T} p | _        | Left q₂  | _        | Right q₄ = CONTRADICTION (<:-impl-¬≮: (<:-trans p q₄) q₂)
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | _        | Right q₃ | _        = CONTRADICTION (<:-impl-¬≮: (<:-trans p q₃) q₁)
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | Right q₂ | Left q₃  | Left q₄  = <:-trans (resolve-<: G p) <:-∪-right
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | Right q₂ | Left q₃  | Right q₄ = resolve-<: G p
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Left q₂  | Left q₃  | Left q₄  = <:-trans (resolve-<: F p) <:-∪-left
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Left q₂  | Right q₃ | Left q₄  = resolve-<: F p
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Left q₃  | Left q₄  = <:-trans <:-∩-left (<:-trans (resolve-<: F p) <:-∪-left)
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Left q₃  | Right q₄ = <:-trans <:-∩-right (resolve-<: G p)
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Right q₃ | Left q₄  = <:-trans <:-∩-left (resolve-<: F p)
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Right q₃ | Right q₄ = <:-intersect (resolve-<: F p) (resolve-<: G p)

-- A function type is a subtype of any of its overloadings
resolve-intro : ∀ F V → (V ≮: never) → (V <: src F) → F <: (V ⇒ resolve F V)
resolve-intro nil V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro (S ⇒ T) V p₁ p₂ = <:-function p₂ <:-refl
resolve-intro never V p₁ p₂ = <:-never
resolve-intro unknown V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro boolean V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro number V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro string V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro (F ∪ G) V p₁ p₂ = <:-∪-lub (<:-trans (resolve-intro F V p₁ (<:-trans p₂ <:-∩-left)) (<:-function <:-refl <:-∪-left)) (<:-trans (resolve-intro G V p₁ (<:-trans p₂ <:-∩-right)) (<:-function <:-refl <:-∪-right))
resolve-intro (F ∩ G) V p₁ p₂ with dec-subtyping V (src F) | dec-subtyping V (src G)
resolve-intro (F ∩ G) V p₁ p₂ | Left q₁ | Left q₂ = <:-trans (<:-intersect (resolve-intro F (V ∩ src F) (<:-never-right p₂ q₂) <:-∩-right) ((resolve-intro G (V ∩ src G) (<:-never-left p₂ q₁) <:-∩-right))) (<:-trans <:-function-∩-∪ (<:-function (<:-trans (<:-∩-glb <:-refl p₂) <:-∩-dist-∪) (<:-union (resolve-<: F <:-∩-left) (resolve-<: G <:-∩-left))))
resolve-intro (F ∩ G) V p₁ p₂ | Left q₁ | Right q₂ = <:-trans <:-∩-right (resolve-intro G V p₁ q₂)
resolve-intro (F ∩ G) V p₁ p₂ | Right q₁ | Left q₂ = <:-trans <:-∩-left (resolve-intro F V p₁ q₁)
resolve-intro (F ∩ G) V p₁ p₂ | Right q₁ | Right q₂ = <:-trans (<:-intersect (resolve-intro F V p₁ q₁) (resolve-intro G V p₁ q₂)) <:-function-∩

resolve⁻¹ : Type → Type → Type
resolve⁻¹ nil U = unknown
resolve⁻¹ (S ⇒ T) U with dec-subtyping T U
resolve⁻¹ (S ⇒ T) U | Left p = never
resolve⁻¹ (S ⇒ T) U | Right p = S
resolve⁻¹ never U = {!!}
resolve⁻¹ unknown U = {!!}
resolve⁻¹ boolean U = {!!}
resolve⁻¹ number U = {!!}
resolve⁻¹ string U = {!!}
resolve⁻¹ (F ∪ G) U = resolve⁻¹ F U ∩ resolve⁻¹ G U
resolve⁻¹ (F ∩ G) U = {!!}

uuu : ∀ F U → (resolve⁻¹ F U <: src F)
uuu = ?

www : ∀ F U V → (V ≮: never) → (V <: src F) → (resolve F V ≮: U) → (V ≮: resolve⁻¹ F U)
www nil U V p q w = CONTRADICTION (<:-impl-¬≮: <:-never w)
www (S ⇒ T) U V p q w with dec-subtyping T U
www (S ⇒ T) U V p q w | Left r = p
www (S ⇒ T) U V p q w | Right r = CONTRADICTION (<:-impl-¬≮: r w)
www never U V p q w = {!!}
www unknown U V p q w = {!!}
www boolean U V p q w = {!!}
www number U V p q w = {!!}
www string U V p q w = {!!}
www (F ∪ G) U V p q w = {!!}
www (F ∩ G) U V p q w with dec-subtyping V (src F) | dec-subtyping V (src G)
www (F ∩ G) U V p q w | Left r₁ | Left r₂ = {!!}
www (F ∩ G) U V p q w | Left r₁ | Right x = {!!}
www (F ∩ G) U V p q w | Right x | y = {!!}

xxx : ∀ F U V → (V ≮: never) → (resolve F V ≮: U) → (V ≮: resolve⁻¹ F U)
xxx nil U V p q = CONTRADICTION (<:-impl-¬≮: <:-never q)
xxx (S ⇒ T) U V p q with dec-subtyping T U
xxx (S ⇒ T) U V p q | Left r = p
xxx (S ⇒ T) U V p q | Right r = CONTRADICTION (<:-impl-¬≮: r q)
xxx never U V p q = {!!}
xxx unknown U V p q = {!!}
xxx boolean U V p q = {!!}
xxx number U V p q = {!!}
xxx string U V p q = {!!}
xxx (F ∪ G) U V p (witness t (left q) r) = ≮:-∩-left (xxx F U V p (witness t q r))
xxx (F ∪ G) U V p (witness t (right q) r) = ≮:-∩-right (xxx G U V p (witness t q r))
xxx (F ∩ G) U V p q with dec-subtyping V (src F) | dec-subtyping V (src G)
xxx (F ∩ G) U V p (witness t (left q₁) q₂) | Left r₁ | Left r₂ = {!xxx F U V p (witness t q₁ q₂)!}
xxx (F ∩ G) U V p (witness t (right q₁) q₂) | Left r₁ | Left r₂ = {!!}
xxx (F ∩ G) U V p q | Left r₁ | Right r₂ = {!!}
xxx (F ∩ G) U V p q | Right x | r₃ = {!!}

yyy : ∀ F U V → (V ≮: resolve⁻¹ F U) → (resolve F V ≮: U)
yyy nil U V w = {!w!}
yyy (S ⇒ T) U V w = {!!}
yyy never U V w = {!!}
yyy unknown U V w = {!!}
yyy boolean U V w = {!!}
yyy number U V w = {!!}
yyy string U V w = {!!}
yyy (F ∪ G) U V (witness t p (left r)) = ≮:-∪-left (yyy F U V (witness t p r))
yyy (F ∪ G) U V (witness t p (right r)) = ≮:-∪-right (yyy G U V (witness t p r))
yyy (F ∩ F₁) U V w = {!!}
