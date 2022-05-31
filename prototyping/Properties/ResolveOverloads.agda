{-# OPTIONS --rewriting #-}

module Properties.ResolveOverloads where

open import FFI.Data.Either using (Left; Right)
open import Luau.ResolveOverloads using (Resolved; src; srcⁿ; resolve; resolveⁿ; resolveᶠ; resolveˢ; target; yes; no)
open import Luau.Subtyping using (_<:_; _≮:_; Language; ¬Language; witness; scalar; unknown; never; function; function-ok; function-err; function-tgt; function-scalar; function-ok₁; function-ok₂; scalar-scalar; scalar-function; scalar-function-ok; scalar-function-err; scalar-function-tgt; _,_; left; right)
open import Luau.Type using (Type ; Scalar; _⇒_; _∩_; _∪_; nil; boolean; number; string; unknown; never)
open import Luau.TypeSaturation using (saturate)
open import Luau.TypeNormalization using (normalize)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.DecSubtyping using (dec-subtyping; dec-subtypingⁿ; <:-impl-<:ᵒ)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; <:-trans-≮:; ≮:-trans-<:; <:-∩-left; <:-∩-right; <:-∩-glb; <:-impl-¬≮:; <:-unknown; <:-function; function-≮:-never; <:-never; unknown-≮:-function; scalar-≮:-function; ≮:-∪-right; scalar-≮:-never; <:-∪-left; <:-∪-right; <:-impl-⊇; language-comp)
open import Properties.TypeNormalization using (Normal; FunType; normal; _⇒_; _∩_; _∪_; never; unknown; <:-normalize; normalize-<:; fun-≮:-never; unknown-≮:-fun; scalar-≮:-fun)
open import Properties.TypeSaturation using (Overloads; Saturated; _⊆ᵒ_; _<:ᵒ_; normal-saturate; saturated; <:-saturate; saturate-<:; defn; here; left; right)

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
¬fun-scalar s (S ⇒ T) (function-ok₁ p) = scalar-function-ok s
¬fun-scalar s (S ⇒ T) (function-ok₂ p) = scalar-function-ok s
¬fun-scalar s (S ⇒ T) (function-err p) = scalar-function-err s
¬fun-scalar s (S ⇒ T) (function-tgt p) = scalar-function-tgt s
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
unknown-src-≮: r (witness (function-ok s .(scalar s₁)) p (function-ok x (scalar-scalar s₁ () x₂)))
unknown-src-≮: r (witness (function-ok s .function) p (function-ok x (scalar-function ())))
unknown-src-≮: r (witness (function-ok s .(function-ok _ _)) p (function-ok x (scalar-function-ok ())))
unknown-src-≮: r (witness (function-ok s .(function-err _)) p (function-ok x (scalar-function-err ())))
unknown-src-≮: r (witness (function-err t) p (function-err q)) = witness t q (src-¬function-err p)
unknown-src-≮: r (witness (function-tgt t) p (function-tgt (scalar-function-tgt ())))

-- Properties of resolve
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
