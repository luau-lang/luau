{-# OPTIONS --rewriting #-}

module Properties.DecSubtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.FunctionTypes using (src; srcⁿ; tgt)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_)
open import Properties.Contradiction using (CONTRADICTION; ¬)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; ≮:-trans-<:; <:-trans-≮:; <:-never; <:-unknown; <:-∪-left; <:-∪-right; <:-∪-lub;  ≮:-∪-left; ≮:-∪-right; <:-∩-left; <:-∩-right; <:-∩-glb;  ≮:-∩-left; ≮:-∩-right; dec-language; scalar-<:; <:-everything; <:-function; ≮:-function-left; ≮:-function-right)
open import Properties.TypeNormalization using (FunType; Normal; never; unknown; _∩_; _∪_; _⇒_; normal; <:-normalize; normalize-<:)
open import Properties.FunctionTypes using (fun-¬scalar; ¬fun-scalar; fun-function; src-unknown-≮:; tgt-never-≮:; src-tgtᶠ-<:)
open import Properties.Equality using (_≢_)

-- Honest this terminates, since src and tgt reduce the depth of nested arrows
{-# TERMINATING #-}
dec-subtypingˢⁿ : ∀ {T U} → Scalar T → Normal U → Either (T ≮: U) (T <: U)
dec-subtypingᶠ : ∀ {T U} → FunType T → FunType U → Either (T ≮: U) (T <: U)
dec-subtypingᶠⁿ : ∀ {T U} → FunType T → Normal U → Either (T ≮: U) (T <: U)
dec-subtypingⁿ : ∀ {T U} → Normal T → Normal U → Either (T ≮: U) (T <: U)
dec-subtyping : ∀ T U → Either (T ≮: U) (T <: U)

dec-subtypingˢⁿ T U with dec-language _ (scalar T)
dec-subtypingˢⁿ T U | Left p = Left (witness (scalar T) (scalar T) p)
dec-subtypingˢⁿ T U | Right p = Right (scalar-<: T p)

dec-subtypingᶠ {T = T} _ (U ⇒ V) with dec-subtypingⁿ U (normal (src T)) | dec-subtypingⁿ (normal (tgt T)) V
dec-subtypingᶠ {T = T} _ (U ⇒ V) | Left p | q = Left (≮:-trans-<: (src-unknown-≮: (≮:-trans-<: p (<:-normalize (src T)))) (<:-function <:-refl <:-unknown))
dec-subtypingᶠ {T = T} _ (U ⇒ V) | Right p | Left q = Left (≮:-trans-<: (tgt-never-≮: (<:-trans-≮: (normalize-<: (tgt T)) q)) (<:-trans (<:-function <:-never <:-refl) <:-∪-right))
dec-subtypingᶠ T (U ⇒ V) | Right p | Right q = Right (src-tgtᶠ-<: T (<:-trans p (normalize-<: _)) (<:-trans (<:-normalize _) q))

dec-subtypingᶠ T (U ∩ V) with dec-subtypingᶠ T U | dec-subtypingᶠ T V
dec-subtypingᶠ T (U ∩ V) | Left p | q = Left (≮:-∩-left p)
dec-subtypingᶠ T (U ∩ V) | Right p | Left q = Left (≮:-∩-right q)
dec-subtypingᶠ T (U ∩ V) | Right p | Right q = Right (<:-∩-glb p q)

dec-subtypingᶠⁿ T never = Left (witness function (fun-function T) never)
dec-subtypingᶠⁿ T unknown = Right <:-unknown
dec-subtypingᶠⁿ T (U ⇒ V) = dec-subtypingᶠ T (U ⇒ V)
dec-subtypingᶠⁿ T (U ∩ V) = dec-subtypingᶠ T (U ∩ V)
dec-subtypingᶠⁿ T (U ∪ V) with dec-subtypingᶠⁿ T U
dec-subtypingᶠⁿ T (U ∪ V) | Left (witness t p q) = Left (witness t p (q , ¬fun-scalar V T p))
dec-subtypingᶠⁿ T (U ∪ V) | Right p = Right (<:-trans p <:-∪-left)

dec-subtypingⁿ never U = Right <:-never
dec-subtypingⁿ unknown unknown = Right <:-refl
dec-subtypingⁿ unknown U with dec-subtypingᶠⁿ (never ⇒ unknown) U
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

dec-subtyping T U with dec-subtypingⁿ (normal T) (normal U)
dec-subtyping T U | Left p = Left (<:-trans-≮: (normalize-<: T) (≮:-trans-<: p (<:-normalize U)))
dec-subtyping T U | Right p = Right (<:-trans (<:-normalize T) (<:-trans p (normalize-<: U)))

