{-# OPTIONS --rewriting #-}

module Properties.OverloadedFunctions where

open import FFI.Data.Either using (Either; Left; Right)
open import Luau.OverloadedFunctions using (resolve)
open import Luau.Subtyping using (_<:_; _≮:_)
open import Luau.Type using (Type; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src)
open import Properties.Subtyping using (dec-subtyping)

-- Function overload resolution respects subtyping

resolve-<: : ∀ F {S T} → (S <: T) → (resolve F S <: resolve F T)
resolve-<: nil p = <:-refl
resolve-<: (F ⇒ F₁) p = <:-refl
resolve-<: never p = <:-refl
resolve-<: unknown p = <:-refl
resolve-<: boolean p = <:-refl
resolve-<: number p = <:-refl
resolve-<: string p = <:-refl
resolve-<: (F ∪ G) p = ∪-<: (resolve-<: F p) (resolve-<: G p)
resolve-<: (F ∩ G) {S} {T} p with dec-subtyping S (src F) | dec-subtyping S (src G) | dec-subtyping T (src F) | dec-subtyping T (src G)
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | Left q₂  | Left q₃  | Left q₄  = ∪-<: (resolve-<: F p) (resolve-<: G p)
resolve-<: (F ∩ G) {S} {T} p | _        | Left q₂  | _        | Right q₄ = CONTRADICTION (<:-impl-¬≮: (<:-trans p q₄) q₂)
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | _        | Right q₃ | _        = CONTRADICTION (<:-impl-¬≮: (<:-trans p q₃) q₁)
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | Right q₂ | Left q₃  | Left q₄  = <:-trans (resolve-<: G p) <:-∪-right
resolve-<: (F ∩ G) {S} {T} p | Left q₁  | Right q₂ | Left q₃  | Right q₄ = resolve-<: G p
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Left q₂  | Left q₃  | Left q₄  = <:-trans (resolve-<: F p) <:-∪-left
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Left q₂  | Right q₃ | Left q₄  = resolve-<: F p
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Left q₃  | Left q₄  = <:-trans <:-∩-left (<:-trans (resolve-<: F p) <:-∪-left)
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Left q₃  | Right q₄ = <:-trans <:-∩-right (resolve-<: G p)
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Right q₃ | Left q₄  = <:-trans <:-∩-left (resolve-<: F p)
resolve-<: (F ∩ G) {S} {T} p | Right q₁ | Right q₂ | Right q₃ | Right q₄ = ∩-<: (resolve-<: F p) (resolve-<: G p)

-- A function type is a subtype of any of its overloadings
resolve-intro : ∀ F V → (V ≮: never) → (V <: src F) → F <: (V ⇒ resolve F V)
resolve-intro nil V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro (S ⇒ T) V p₁ p₂ = function-<: p₂ <:-refl
resolve-intro never V p₁ p₂ = <:-never
resolve-intro unknown V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro boolean V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro number V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro string V p₁ p₂ = CONTRADICTION (<:-impl-¬≮: p₂ p₁)
resolve-intro (F ∪ G) V p₁ p₂ = <:-∪-lub (<:-trans (resolve-intro F V p₁ (<:-trans p₂ <:-∩-left)) (function-<: <:-refl <:-∪-left)) (<:-trans (resolve-intro G V p₁ (<:-trans p₂ <:-∩-right)) (function-<: <:-refl <:-∪-right))
resolve-intro (F ∩ G) V p₁ p₂ with dec-subtyping V (src F) | dec-subtyping V (src G)
resolve-intro (F ∩ G) V p₁ p₂ | Left q₁ | Left q₂ = <:-trans (∩-<: (resolve-intro F (V ∩ src F) (<:-never-right p₂ q₂) <:-∩-right) ((resolve-intro G (V ∩ src G) (<:-never-left p₂ q₁) <:-∩-right))) (<:-trans function-∩-∪-<: (function-<: (<:-trans (<:-∩-glb <:-refl p₂) <:-∩-dist-∪) (∪-<: (resolve-<: F <:-∩-left) (resolve-<: G <:-∩-left))))
resolve-intro (F ∩ G) V p₁ p₂ | Left q₁ | Right q₂ = <:-trans <:-∩-right (resolve-intro G V p₁ q₂)
resolve-intro (F ∩ G) V p₁ p₂ | Right q₁ | Left q₂ = <:-trans <:-∩-left (resolve-intro F V p₁ q₁)
resolve-intro (F ∩ G) V p₁ p₂ | Right q₁ | Right q₂ = <:-trans (∩-<: (resolve-intro F V p₁ q₁) (resolve-intro G V p₁ q₂)) function-∩-<:
