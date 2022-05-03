{-# OPTIONS --rewriting #-}
{-# OPTIONS --allow-unsolved-metas #-}

module Properties.DecSubtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-ok₁; function-ok₂; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_)
open import Luau.TypeNormalization using (_∪ⁿ_; _∩ⁿ_)
open import Properties.Contradiction using (CONTRADICTION; ¬)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; ≮:-trans-<:; <:-trans-≮:; <:-never; <:-unknown; <:-∪-left; <:-∪-right; <:-∪-lub;  ≮:-∪-left; ≮:-∪-right; <:-∩-left; <:-∩-right; <:-∩-glb;  ≮:-∩-left; ≮:-∩-right; dec-language; scalar-<:; <:-everything; <:-function; ≮:-function-left; ≮:-function-right; <:-impl-¬≮:; <:-intersect; <:-function-∩-∪; <:-function-∩; <:-union; ≮:-left-∪; ≮:-right-∪; <:-∩-distr-∪; <:-impl-⊇; language-comp)
open import Properties.TypeNormalization using (FunType; Normal; never; unknown; function; _∩_; _∪_; _⇒_; normal; <:-normalize; normalize-<:; normal-∩ⁿ; normal-∪ⁿ; ∪-<:-∪ⁿ; ∪ⁿ-<:-∪; ∩ⁿ-<:-∩; ∩-<:-∩ⁿ; normalⁱ; normalᶠ)
open import Properties.Equality using (_≢_)

fun-top : ∀ {F} → (FunType F) → (F <: (never ⇒ unknown))
fun-top function = <:-refl
fun-top (S ⇒ T) = <:-function <:-never <:-unknown
fun-top (F ∩ G) = <:-trans <:-∩-left (fun-top F)

fun-function : ∀ {F} → FunType F → Language F function
fun-function function = function
fun-function (S ⇒ T) = function
fun-function (F ∩ G) = (fun-function F , fun-function G)

¬fun-scalar : ∀ {F S} → (s : Scalar S) → FunType F → ¬Language F (scalar s)
¬fun-scalar = {!!}

fun-¬scalar : ∀ {F S t} → (s : Scalar S) → FunType F → Language F t → ¬Language S t
fun-¬scalar s function function = scalar-function s
fun-¬scalar s function (function-ok₁ p) = scalar-function-ok s
fun-¬scalar s function (function-ok₂ p) = scalar-function-ok s
fun-¬scalar s function (function-err p) = scalar-function-err s
fun-¬scalar s (S ⇒ T) function = scalar-function s
fun-¬scalar s (S ⇒ T) (function-ok₁ p) = scalar-function-ok s
fun-¬scalar s (S ⇒ T) (function-ok₂ p) = scalar-function-ok s
fun-¬scalar s (S ⇒ T) (function-err p) = scalar-function-err s
fun-¬scalar s (F ∩ G) (p₁ , p₂) = fun-¬scalar s G p₂

function-err-ok : ∀ {F s t} → FunType F → Language F (function-err s) → Language F (function-ok s t)
function-err-ok = {!!}

-- Honest this terminates, since src and tgt reduce the depth of nested arrows
{-# TERMINATING #-}
dec-subtypingˢⁿ : ∀ {T U} → Scalar T → Normal U → Either (T ≮: U) (T <: U)
dec-subtypingᶠ : ∀ {T U} → FunType T → FunType U → Either (T ≮: U) (T <: U)
dec-subtypingᶠⁿ : ∀ {T U} → FunType T → Normal U → Either (T ≮: U) (T <: U)
dec-subtypingⁿ : ∀ {T U} → Normal T → Normal U → Either (T ≮: U) (T <: U)
dec-subtyping : ∀ T U → Either (T ≮: U) (T <: U)

srcᶠ : ∀ {F} → FunType F → Type
normal-srcᶠ : ∀ {F} → (Fᶠ : FunType F) → Normal (srcᶠ Fᶠ)
srcᶠ-function-err : ∀ {F} → (Fᶠ : FunType F) → ∀ s → ¬Language (srcᶠ Fᶠ) s → Language F (function-err s)
≮:-srcᶠ : ∀ {F S T} → (Fᶠ : FunType F) → (S ≮: srcᶠ Fᶠ) → (F ≮: (S ⇒ T))

resolveᶠⁿ : ∀ {F V} → FunType F → Normal V → Type
normal-resolveᶠⁿ : ∀ {F V} → (Fᶠ : FunType F) → (Vⁿ : Normal V) → Normal (resolveᶠⁿ Fᶠ Vⁿ)
-- function-ok-resolveᶠⁿ : ∀ {F V} → (Fᶠ : FunType F) → (Vⁿ : Normal V) → (V <: srcᶠ Fᶠ) → ∀ s t → Language V s → Language F (function-ok s t) → Language (resolveᶠⁿ Fᶠ Vⁿ) t
≮:-resolveᶠⁿ : ∀ {F S T} → (Fᶠ : FunType F) → (Sᶠ : Normal S) → (S <: srcᶠ Fᶠ) → (S ≮: never) → (resolveᶠⁿ Fᶠ Sᶠ ≮: T) → (F ≮: (S ⇒ T))
<:-resolveᶠⁿ : ∀ {F S} → (Fᶠ : FunType F) → (Sⁿ : Normal S) → (S <: srcᶠ Fᶠ) → (F <: (S ⇒ resolveᶠⁿ Fᶠ Sⁿ))

srcᶠ {S ⇒ T} F = S
srcᶠ (F ∩ G) = srcᶠ F ∪ⁿ srcᶠ G

normal-srcᶠ function = never
normal-srcᶠ (S ⇒ T) = normalⁱ S
normal-srcᶠ (F ∩ G) = normal-∪ⁿ (normal-srcᶠ F) (normal-srcᶠ G)

srcᶠ-function-err function s p = function-err p
srcᶠ-function-err (S ⇒ T) s p = function-err p
srcᶠ-function-err (F ∩ G) s p with <:-impl-⊇ (∪-<:-∪ⁿ (normal-srcᶠ F) (normal-srcᶠ G)) s p
srcᶠ-function-err (F ∩ G) s p | (p₁ , p₂) = (srcᶠ-function-err F s p₁ , srcᶠ-function-err G s p₂)

≮:-srcᶠ F (witness s p q) = witness (function-err s) (srcᶠ-function-err F s q) (function-err p)

resolveᶠⁿ {S ⇒ T} F V = T
resolveᶠⁿ (F ∩ G) V with dec-subtypingⁿ V (normal-srcᶠ F) | dec-subtypingⁿ V (normal-srcᶠ G)
resolveᶠⁿ (F ∩ G) V | Left p | Left q = resolveᶠⁿ F (normal-∩ⁿ (normal-srcᶠ F) V) ∪ⁿ resolveᶠⁿ G (normal-∩ⁿ (normal-srcᶠ G) V)
resolveᶠⁿ (F ∩ G) V | Left p | Right q = resolveᶠⁿ G V
resolveᶠⁿ (F ∩ G) V | Right p | Left q = resolveᶠⁿ F V
resolveᶠⁿ (F ∩ G) V | Right p | Right q = resolveᶠⁿ F V ∩ⁿ resolveᶠⁿ G V

normal-resolveᶠⁿ function V = unknown
normal-resolveᶠⁿ (S ⇒ T) V = T
normal-resolveᶠⁿ (F ∩ G) V with dec-subtypingⁿ V (normal-srcᶠ F) | dec-subtypingⁿ V (normal-srcᶠ G)
normal-resolveᶠⁿ (F ∩ G) V | Left p | Left q = normal-∪ⁿ (normal-resolveᶠⁿ F (normal-∩ⁿ (normal-srcᶠ F) V)) (normal-resolveᶠⁿ G (normal-∩ⁿ (normal-srcᶠ G) V))
normal-resolveᶠⁿ (F ∩ G) V | Left p | Right q = normal-resolveᶠⁿ G V
normal-resolveᶠⁿ (F ∩ G) V | Right p | Left q = normal-resolveᶠⁿ F V
normal-resolveᶠⁿ (F ∩ G) V | Right p | Right q = normal-∩ⁿ (normal-resolveᶠⁿ F V) (normal-resolveᶠⁿ G V)

-- function-ok-resolveᶠⁿ function V p s t q₁ q₂ = {!!}
-- function-ok-resolveᶠⁿ (S ⇒ T) V p s t q₁ q₂ = {!!}
-- function-ok-resolveᶠⁿ (F ∩ G) V p s t q₁ (q₂ , q₃) with dec-subtypingⁿ V (normal-srcᶠ F) | dec-subtypingⁿ V (normal-srcᶠ G)
-- function-ok-resolveᶠⁿ (F ∩ G) V p s t q₁ (q₂ , q₃) | Left r₁ | Left r₂ with ∪ⁿ-<:-∪ (normal-srcᶠ F) (normal-srcᶠ G) s (p s q₁)
-- function-ok-resolveᶠⁿ (F ∩ G) V p s t q₁ (q₂ , q₃) | Left r₁ | Left r₂ | left r₃ = ∪-<:-∪ⁿ (normal-resolveᶠⁿ F (normal-∩ⁿ (normal-srcᶠ F) V))
--                                                                                      (normal-resolveᶠⁿ G (normal-∩ⁿ (normal-srcᶠ G) V)) t (left {!function-ok-resolveᶠⁿ F (normal-∩ⁿ (normal-srcᶠ F) V) ? s t ? !}) -- ∪-<:-∪ⁿ (normal-resolveᶠⁿ F V) {!normal-resolveᶠⁿ G V!} t (left (function-ok-resolveᶠⁿ F V {!!} s t q₁ q₂))


<:-resolveᶠⁿ function V p = <:-function p <:-refl
<:-resolveᶠⁿ (S ⇒ T) V p = <:-function p <:-refl
<:-resolveᶠⁿ (F ∩ G) V p with dec-subtypingⁿ V (normal-srcᶠ F) | dec-subtypingⁿ V (normal-srcᶠ G)
<:-resolveᶠⁿ {F ∩ G} {V} (Fᶠ ∩ Gᶠ) Vⁿ p | Left q | Left r = result where

  T = resolveᶠⁿ Fᶠ (normal-∩ⁿ (normal-srcᶠ Fᶠ) Vⁿ)
  U = resolveᶠⁿ Gᶠ (normal-∩ⁿ (normal-srcᶠ Gᶠ) Vⁿ)
  
  Tⁿ = normal-resolveᶠⁿ Fᶠ (normal-∩ⁿ (normal-srcᶠ Fᶠ) Vⁿ)
  Uⁿ = normal-resolveᶠⁿ Gᶠ (normal-∩ⁿ (normal-srcᶠ Gᶠ) Vⁿ)
  
  lemma₁ : F <: ((srcᶠ Fᶠ ∩ V) ⇒ T)
  lemma₁ = <:-trans (<:-resolveᶠⁿ Fᶠ (normal-∩ⁿ (normal-srcᶠ Fᶠ) Vⁿ) (<:-trans (∩ⁿ-<:-∩ (normal-srcᶠ Fᶠ) Vⁿ) <:-∩-left)) (<:-function (∩-<:-∩ⁿ (normal-srcᶠ Fᶠ) Vⁿ) <:-refl)

  lemma₂ : G <: ((srcᶠ Gᶠ ∩ V) ⇒ U)
  lemma₂ = <:-trans (<:-resolveᶠⁿ Gᶠ (normal-∩ⁿ (normal-srcᶠ Gᶠ) Vⁿ) (<:-trans (∩ⁿ-<:-∩ (normal-srcᶠ Gᶠ) Vⁿ) <:-∩-left)) (<:-function (∩-<:-∩ⁿ (normal-srcᶠ Gᶠ) Vⁿ) <:-refl)

  result : (F ∩ G) <: (V ⇒ (T ∪ⁿ U))
  result = <:-trans (<:-trans (<:-intersect lemma₁ lemma₂) <:-function-∩-∪) (<:-function (<:-trans (<:-∩-glb (<:-trans p (∪ⁿ-<:-∪ (normal-srcᶠ Fᶠ) (normal-srcᶠ Gᶠ))) <:-refl) <:-∩-distr-∪) (∪-<:-∪ⁿ Tⁿ Uⁿ))

<:-resolveᶠⁿ {F ∩ G} {V} (Fᶠ ∩ Gᶠ) Vⁿ p | Left q | Right r = <:-trans <:-∩-right (<:-resolveᶠⁿ Gᶠ Vⁿ r)
<:-resolveᶠⁿ {F ∩ G} {V} (Fᶠ ∩ Gᶠ) Vⁿ p | Right q | Left r = <:-trans <:-∩-left (<:-resolveᶠⁿ Fᶠ Vⁿ q)
<:-resolveᶠⁿ {F ∩ G} {V} (Fᶠ ∩ Gᶠ) Vⁿ p | Right q | Right r = <:-trans (<:-intersect (<:-resolveᶠⁿ Fᶠ Vⁿ q) (<:-resolveᶠⁿ Gᶠ Vⁿ r)) (<:-trans <:-function-∩ (<:-function <:-refl (∩-<:-∩ⁿ (normal-resolveᶠⁿ Fᶠ Vⁿ) (normal-resolveᶠⁿ Gᶠ Vⁿ))))

≮:-resolveᶠⁿ F V p (witness s q₁ q₂) (witness t r₁ r₂) = witness (function-ok s t) {!!} (function-ok q₁ {!r₂!})

dec-subtypingˢⁿ T U with dec-language _ (scalar T)
dec-subtypingˢⁿ T U | Left p = Left (witness (scalar T) (scalar T) p)
dec-subtypingˢⁿ T U | Right p = Right (scalar-<: T p)

dec-subtypingᶠ T function = Right (fun-top T)
dec-subtypingᶠ T (U ⇒ V) with dec-subtypingⁿ (normalⁱ U) (normal-srcᶠ T) | dec-subtypingⁿ (normal-resolveᶠⁿ T (normalⁱ U)) V
dec-subtypingᶠ T (U ⇒ V) | Left p | q = Left (≮:-srcᶠ T p)
dec-subtypingᶠ T (U ⇒ V) | Right p | Left q = Left {!q!} -- Left (<:-trans-≮: {!!} (≮:-function-right {!!} q)) -- (≮:-resolveᶠⁿ T (normalⁱ U) p q)
dec-subtypingᶠ T (U ⇒ V) | Right p | Right q = Right (<:-trans (<:-resolveᶠⁿ T (normalⁱ U) p) (<:-function <:-refl q))
dec-subtypingᶠ T (U ∩ V) with dec-subtypingᶠ T U | dec-subtypingᶠ T V
dec-subtypingᶠ T (U ∩ V) | Left p | q = Left (≮:-∩-left p)
dec-subtypingᶠ T (U ∩ V) | Right p | Left q = Left (≮:-∩-right q)
dec-subtypingᶠ T (U ∩ V) | Right p | Right q = Right (<:-∩-glb p q)

dec-subtypingᶠⁿ T never = Left (witness function (fun-function T) never)
dec-subtypingᶠⁿ T unknown = Right <:-unknown
dec-subtypingᶠⁿ T function = Right (fun-top T)
dec-subtypingᶠⁿ T (U ⇒ V) = dec-subtypingᶠ T (U ⇒ V)
dec-subtypingᶠⁿ T (U ∩ V) = dec-subtypingᶠ T (U ∩ V)
dec-subtypingᶠⁿ T (U ∪ V) with dec-subtypingᶠⁿ T U
dec-subtypingᶠⁿ T (U ∪ V) | Left (witness t p q) = Left (witness t p (q , fun-¬scalar V T p))
dec-subtypingᶠⁿ T (U ∪ V) | Right p = Right (<:-trans p <:-∪-left)

dec-subtypingⁿ never U = Right <:-never
dec-subtypingⁿ unknown unknown = Right <:-refl
dec-subtypingⁿ unknown U with dec-subtypingᶠⁿ function U
dec-subtypingⁿ unknown U | Left p = Left (<:-trans-≮: <:-unknown p)
dec-subtypingⁿ unknown U | Right p₁ with dec-subtypingˢⁿ number U
dec-subtypingⁿ unknown U | Right p₁ | Left p = Left (<:-trans-≮: <:-unknown p)
dec-subtypingⁿ unknown U | Right p₁ | Right p₂ with dec-subtypingˢⁿ string U
dec-subtypingⁿ unknown U | Right p₁ | Right p₂ | Left p = Left (<:-trans-≮: <:-unknown p)
dec-subtypingⁿ unknown U | Right p₁ | Right p₂ | Right p₃ with dec-subtypingˢⁿ nil U
dec-subtypingⁿ unknown U | Right p₁ | Right p₂ | Right p₃ | Left p = Left (<:-trans-≮: <:-unknown p)
dec-subtypingⁿ unknown U | Right p₁ | Right p₂ | Right p₃ | Right p₄ with dec-subtypingˢⁿ boolean U
dec-subtypingⁿ unknown U | Right p₁ | Right p₂ | Right p₃ | Right p₄ | Left p = Left (<:-trans-≮: <:-unknown p)
dec-subtypingⁿ unknown U | Right p₁ | Right p₂ | Right p₃ | Right p₄ | Right p₅ = Right (<:-trans <:-everything (<:-∪-lub p₁ (<:-∪-lub p₂ (<:-∪-lub p₃ (<:-∪-lub p₄ p₅)))))
dec-subtypingⁿ (S ⇒ T) U = dec-subtypingᶠⁿ (S ⇒ T) U
dec-subtypingⁿ (S ∩ T) U = dec-subtypingᶠⁿ (S ∩ T) U
dec-subtypingⁿ (S ∪ T) U with dec-subtypingⁿ S U | dec-subtypingˢⁿ T U
dec-subtypingⁿ (S ∪ T) U | Left p | q = Left (≮:-∪-left p)
dec-subtypingⁿ (S ∪ T) U | Right p | Left q = Left (≮:-∪-right q)
dec-subtypingⁿ (S ∪ T) U | Right p | Right q = Right (<:-∪-lub p q)
dec-subtypingⁿ function function = Right <:-refl
dec-subtypingⁿ function (T ⇒ U) = dec-subtypingᶠ function (T ⇒ U)
dec-subtypingⁿ function (T ∩ U) = dec-subtypingᶠ function (T ∩ U)
dec-subtypingⁿ function (T ∪ U) = dec-subtypingᶠⁿ function (T ∪ U)
dec-subtypingⁿ function never = Left (witness function function never)
dec-subtypingⁿ function unknown = Right <:-unknown

dec-subtyping T U with dec-subtypingⁿ (normal T) (normal U)
dec-subtyping T U | Left p = Left (<:-trans-≮: (normalize-<: T) (≮:-trans-<: p (<:-normalize U)))
dec-subtyping T U | Right p = Right (<:-trans (<:-normalize T) (<:-trans p (normalize-<: U)))
