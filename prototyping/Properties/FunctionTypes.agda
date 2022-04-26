{-# OPTIONS --rewriting #-}

module Properties.FunctionTypes where

open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.FunctionTypes using (srcⁿ; src; tgt)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; skalar)
open import Properties.Contradiction using (CONTRADICTION; ¬; ⊥)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; ≮:-refl; <:-trans-≮:; skalar-scalar; <:-impl-⊇; skalar-function-ok; language-comp)
open import Properties.TypeNormalization using (FunType; Normal; never; unknown; _∩_; _∪_; _⇒_; normal; <:-normalize; normalize-<:)

-- Properties of src
function-err-srcⁿ : ∀ {T t} → (FunType T) → (¬Language (srcⁿ T) t) → Language T (function-err t)
function-err-srcⁿ (S ⇒ T) p = function-err p
function-err-srcⁿ (S ∩ T) (p₁ , p₂) = (function-err-srcⁿ S p₁ , function-err-srcⁿ T p₂)

¬function-err-srcᶠ : ∀ {T t} → (FunType T) → (Language (srcⁿ T) t) → ¬Language T (function-err t)
¬function-err-srcᶠ (S ⇒ T) p = function-err p
¬function-err-srcᶠ (S ∩ T) (left p) = left (¬function-err-srcᶠ S p)
¬function-err-srcᶠ (S ∩ T) (right p) = right (¬function-err-srcᶠ T p)

¬function-err-srcⁿ : ∀ {T t} → (Normal T) → (Language (srcⁿ T) t) → ¬Language T (function-err t)
¬function-err-srcⁿ never p = never
¬function-err-srcⁿ unknown (scalar ())
¬function-err-srcⁿ (S ⇒ T) p = function-err p
¬function-err-srcⁿ (S ∩ T) (left p) = left (¬function-err-srcᶠ S p)
¬function-err-srcⁿ (S ∩ T) (right p) = right (¬function-err-srcᶠ T p)
¬function-err-srcⁿ (S ∪ T) (scalar ())

¬function-err-src : ∀ {T t} → (Language (src T) t) → ¬Language T (function-err t)
¬function-err-src {T = S ⇒ T} p = function-err p
¬function-err-src {T = nil} p = scalar-function-err nil
¬function-err-src {T = never} p = never
¬function-err-src {T = unknown} (scalar ())
¬function-err-src {T = boolean} p = scalar-function-err boolean
¬function-err-src {T = number} p = scalar-function-err number
¬function-err-src {T = string} p = scalar-function-err string
¬function-err-src {T = S ∪ T} p = <:-impl-⊇ (<:-normalize (S ∪ T)) _ (¬function-err-srcⁿ (normal (S ∪ T)) p)
¬function-err-src {T = S ∩ T} p = <:-impl-⊇ (<:-normalize (S ∩ T)) _ (¬function-err-srcⁿ (normal (S ∩ T)) p)

src-¬function-errᶠ : ∀ {T t} → (FunType T) → Language T (function-err t) → (¬Language (srcⁿ T) t)
src-¬function-errᶠ (S ⇒ T) (function-err p) = p
src-¬function-errᶠ (S ∩ T) (p₁ , p₂) = (src-¬function-errᶠ S p₁ , src-¬function-errᶠ T p₂)

src-¬function-errⁿ : ∀ {T t} → (Normal T) → Language T (function-err t) → (¬Language (srcⁿ T) t)
src-¬function-errⁿ unknown p = never
src-¬function-errⁿ (S ⇒ T) (function-err p) = p
src-¬function-errⁿ (S ∩ T) (p₁ , p₂) = (src-¬function-errᶠ S p₁ , src-¬function-errᶠ T p₂)
src-¬function-errⁿ (S ∪ T) p = never

src-¬function-err : ∀ {T t} → Language T (function-err t) → (¬Language (src T) t)
src-¬function-err {T = S ⇒ T} (function-err p) = p
src-¬function-err {T = unknown} p = never
src-¬function-err {T = S ∪ T} p = src-¬function-errⁿ (normal (S ∪ T)) (<:-normalize (S ∪ T) _ p)
src-¬function-err {T = S ∩ T} p = src-¬function-errⁿ (normal (S ∩ T)) (<:-normalize (S ∩ T) _ p)

fun-¬scalar : ∀ {S T} (s : Scalar S) → FunType T → ¬Language T (scalar s)
fun-¬scalar s (S ⇒ T) = function-scalar s
fun-¬scalar s (S ∩ T) = left (fun-¬scalar s S)

¬fun-scalar : ∀ {S T t} (s : Scalar S) → FunType T → Language T t → ¬Language S t
¬fun-scalar s (S ⇒ T) function = scalar-function s
¬fun-scalar s (S ⇒ T) (function-ok p) = scalar-function-ok s
¬fun-scalar s (S ⇒ T) (function-err p) = scalar-function-err s
¬fun-scalar s (S ∩ T) (p₁ , p₂) = ¬fun-scalar s T p₂

fun-function : ∀ {T} → FunType T → Language T function
fun-function (S ⇒ T) = function
fun-function (S ∩ T) = (fun-function S , fun-function T)

srcⁿ-¬scalar : ∀ {S T t} (s : Scalar S) → Normal T → Language T (scalar s) → (¬Language (srcⁿ T) t)
srcⁿ-¬scalar s never (scalar ())
srcⁿ-¬scalar s unknown p = never
srcⁿ-¬scalar s (S ⇒ T) (scalar ())
srcⁿ-¬scalar s (S ∩ T) (p₁ , p₂) = CONTRADICTION (language-comp (scalar s) (fun-¬scalar s S) p₁)
srcⁿ-¬scalar s (S ∪ T) p = never

src-¬scalar : ∀ {S T t} (s : Scalar S) → Language T (scalar s) → (¬Language (src T) t)
src-¬scalar {T = nil} s p = never
src-¬scalar {T = T ⇒ U} s (scalar ())
src-¬scalar {T = never} s (scalar ())
src-¬scalar {T = unknown} s p = never
src-¬scalar {T = boolean} s p = never
src-¬scalar {T = number} s p = never
src-¬scalar {T = string} s p = never
src-¬scalar {T = T ∪ U} s p = srcⁿ-¬scalar s (normal (T ∪ U)) (<:-normalize (T ∪ U) (scalar s) p)
src-¬scalar {T = T ∩ U} s p = srcⁿ-¬scalar s (normal (T ∩ U)) (<:-normalize (T ∩ U) (scalar s) p)

srcⁿ-unknown-≮: : ∀ {T U} → (Normal U) → (T ≮: srcⁿ U) → (U ≮: (T ⇒ unknown))
srcⁿ-unknown-≮: never (witness t p q) = CONTRADICTION (language-comp t q unknown)
srcⁿ-unknown-≮: unknown (witness t p q) = witness (function-err t) unknown (function-err p)
srcⁿ-unknown-≮: (U ⇒ V) (witness t p q) = witness (function-err t) (function-err q) (function-err p)
srcⁿ-unknown-≮: (U ∩ V) (witness t p q) = witness (function-err t) (function-err-srcⁿ (U ∩ V) q) (function-err p)
srcⁿ-unknown-≮: (U ∪ V) (witness t p q) = witness (scalar V) (right (scalar V)) (function-scalar V)

src-unknown-≮: : ∀ {T U} → (T ≮: src U) → (U ≮: (T ⇒ unknown))
src-unknown-≮: {U = nil} (witness t p q) = witness (scalar nil) (scalar nil) (function-scalar nil)
src-unknown-≮: {U = T ⇒ U} (witness t p q) = witness (function-err t) (function-err q) (function-err p)
src-unknown-≮: {U = never} (witness t p q) = CONTRADICTION (language-comp t q unknown)
src-unknown-≮: {U = unknown} (witness t p q) = witness (function-err t) unknown (function-err p)
src-unknown-≮: {U = boolean} (witness t p q) = witness (scalar boolean) (scalar boolean) (function-scalar boolean)
src-unknown-≮: {U = number} (witness t p q) = witness (scalar number) (scalar number) (function-scalar number)
src-unknown-≮: {U = string} (witness t p q) = witness (scalar string) (scalar string) (function-scalar string)
src-unknown-≮: {U = T ∪ U} p = <:-trans-≮: (normalize-<: (T ∪ U)) (srcⁿ-unknown-≮: (normal (T ∪ U)) p)
src-unknown-≮: {U = T ∩ U} p = <:-trans-≮: (normalize-<: (T ∩ U)) (srcⁿ-unknown-≮: (normal (T ∩ U)) p)

unknown-src-≮: : ∀ {S T U} → (U ≮: S) → (T ≮: (U ⇒ unknown)) → (U ≮: src T)
unknown-src-≮: (witness t x x₁) (witness (scalar s) p (function-scalar s)) = witness t x (src-¬scalar s p)
unknown-src-≮: r (witness (function-ok (scalar s)) p (function-ok (scalar-scalar s () q)))
unknown-src-≮: r (witness (function-ok (function-ok _)) p (function-ok (scalar-function-ok ())))
unknown-src-≮: r (witness (function-err t) p (function-err q)) = witness t q (src-¬function-err p)

-- Properties of tgt
tgt-function-ok : ∀ {T t} → (Language (tgt T) t) → Language T (function-ok t)
tgt-function-ok {T = nil} (scalar ())
tgt-function-ok {T = T₁ ⇒ T₂} p = function-ok p
tgt-function-ok {T = never} (scalar ())
tgt-function-ok {T = unknown} p = unknown
tgt-function-ok {T = boolean} (scalar ())
tgt-function-ok {T = number} (scalar ())
tgt-function-ok {T = string} (scalar ())
tgt-function-ok {T = T₁ ∪ T₂} (left p) = left (tgt-function-ok p)
tgt-function-ok {T = T₁ ∪ T₂} (right p) = right (tgt-function-ok p)
tgt-function-ok {T = T₁ ∩ T₂} (p₁ , p₂) = (tgt-function-ok p₁ , tgt-function-ok p₂)

function-ok-tgt : ∀ {T t} → Language T (function-ok t) → (Language (tgt T) t)
function-ok-tgt (function-ok p) = p
function-ok-tgt (left p) = left (function-ok-tgt p)
function-ok-tgt (right p) = right (function-ok-tgt p)
function-ok-tgt (p₁ , p₂) = (function-ok-tgt p₁ , function-ok-tgt p₂)
function-ok-tgt unknown = unknown

tgt-never-≮: : ∀ {T U} → (tgt T ≮: U) → (T ≮: (skalar ∪ (never ⇒ U)))
tgt-never-≮: (witness t p q) = witness (function-ok t) (tgt-function-ok p) (skalar-function-ok , function-ok q)

never-tgt-≮: : ∀ {T U} → (T ≮: (skalar ∪ (never ⇒ U))) → (tgt T ≮: U)
never-tgt-≮: (witness (scalar s) p (q₁ , q₂)) = CONTRADICTION (≮:-refl (witness (scalar s) (skalar-scalar s) q₁))
never-tgt-≮: (witness function p (q₁ , scalar-function ()))
never-tgt-≮: (witness (function-ok t) p (q₁ , function-ok q₂)) = witness t (function-ok-tgt p) q₂
never-tgt-≮: (witness (function-err (scalar s)) p (q₁ , function-err (scalar ())))

src-tgtᶠ-<: : ∀ {T U V} → (FunType T) → (U <: src T) → (tgt T <: V) → (T <: (U ⇒ V))
src-tgtᶠ-<: T p q (scalar s) r = CONTRADICTION (language-comp (scalar s) (fun-¬scalar s T) r)
src-tgtᶠ-<: T p q function r = function
src-tgtᶠ-<: T p q (function-ok s) r = function-ok (q s (function-ok-tgt r))
src-tgtᶠ-<: T p q (function-err s) r = function-err (<:-impl-⊇ p s (src-¬function-err r))


