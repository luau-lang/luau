{-# OPTIONS --rewriting #-}

module Properties.DecSubtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-function-tgt; scalar-scalar; function-scalar; function-ok; function-ok₁; function-ok₂; function-err; function-tgt; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_)
open import Luau.TypeNormalization using (_∪ⁿ_; _∩ⁿ_)
open import Luau.TypeSaturation using (saturate)
open import Properties.Contradiction using (CONTRADICTION; ¬)
open import Properties.Functions using (_∘_)
open import Properties.Subtyping using (<:-refl; <:-trans; ≮:-trans-<:; <:-trans-≮:; <:-never; <:-unknown; <:-∪-left; <:-∪-right; <:-∪-lub;  ≮:-∪-left; ≮:-∪-right; <:-∩-left; <:-∩-right; <:-∩-glb;  ≮:-∩-left; ≮:-∩-right; dec-language; scalar-<:; <:-everything; <:-function; ≮:-function-left; ≮:-function-right; <:-impl-¬≮:; <:-intersect; <:-function-∩-∪; <:-function-∩; <:-union; ≮:-left-∪; ≮:-right-∪; <:-∩-distr-∪; <:-impl-⊇; language-comp)
open import Properties.TypeNormalization using (FunType; Normal; never; unknown; _∩_; _∪_; _⇒_; normal; <:-normalize; normalize-<:; normal-∩ⁿ; normal-∪ⁿ; ∪-<:-∪ⁿ; ∪ⁿ-<:-∪; ∩ⁿ-<:-∩; ∩-<:-∩ⁿ; normalᶠ; fun-top; fun-function; fun-¬scalar)
open import Properties.TypeSaturation using (Overloads; Saturated; _⊆ᵒ_; _<:ᵒ_; defn; here; left; right; ov-language; ov-<:; saturated; normal-saturate; normal-overload-src; normal-overload-tgt; saturate-<:; <:-saturate; <:ᵒ-impl-<:; _>>=ˡ_; _>>=ʳ_)
open import Properties.Equality using (_≢_)

-- Honest this terminates, since saturation maintains the depth of nested arrows
{-# TERMINATING #-}
dec-subtypingˢⁿ : ∀ {T U} → Scalar T → Normal U → Either (T ≮: U) (T <: U)
dec-subtypingˢᶠ : ∀ {F G} → FunType F → Saturated F → FunType G → Either (F ≮: G) (F <:ᵒ G)
dec-subtypingᶠ : ∀ {F G} → FunType F → FunType G → Either (F ≮: G) (F <: G)
dec-subtypingᶠⁿ : ∀ {F U} → FunType F → Normal U → Either (F ≮: U) (F <: U)
dec-subtypingⁿ : ∀ {T U} → Normal T → Normal U → Either (T ≮: U) (T <: U)
dec-subtyping : ∀ T U → Either (T ≮: U) (T <: U)

dec-subtypingˢⁿ T U with dec-language _ (scalar T)
dec-subtypingˢⁿ T U | Left p = Left (witness (scalar T) (scalar T) p)
dec-subtypingˢⁿ T U | Right p = Right (scalar-<: T p)

dec-subtypingˢᶠ {F} {S ⇒ T} Fᶠ (defn sat-∩ sat-∪) (Sⁿ ⇒ Tⁿ) = result (top Fᶠ (λ o → o)) where

  data Top G : Set where

    defn : ∀ Sᵗ Tᵗ →

      Overloads F (Sᵗ ⇒ Tᵗ) →
      (∀ {S′ T′} → Overloads G (S′ ⇒ T′) → (S′ <: Sᵗ)) →
      -------------
      Top G

  top : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → Top G
  top {S′ ⇒ T′} _ G⊆F = defn S′ T′ (G⊆F here) (λ { here → <:-refl })
  top (Gᶠ ∩ Hᶠ) G⊆F with top Gᶠ (G⊆F ∘ left) | top Hᶠ (G⊆F ∘ right)
  top (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵗ Sᵗ p p₁ | defn Tᵗ Uᵗ q q₁ with sat-∪ p q
  top (Gᶠ ∩ Hᶠ) G⊆F | defn Rᵗ Sᵗ p p₁ | defn Tᵗ Uᵗ q q₁ | defn n r r₁ = defn _ _ n
    (λ { (left o) → <:-trans (<:-trans (p₁ o) <:-∪-left) r ; (right o) → <:-trans (<:-trans (q₁ o) <:-∪-right) r })

  result : Top F → Either (F ≮: (S ⇒ T)) (F <:ᵒ (S ⇒ T))
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) with dec-subtypingⁿ Sⁿ (normal-overload-src Fᶠ oᵗ)
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) | Left (witness s Ss ¬Sᵗs) = Left (witness (function-err s) (ov-language Fᶠ (λ o → function-err (<:-impl-⊇ (srcᵗ o) s ¬Sᵗs))) (function-err Ss))
  result (defn Sᵗ Tᵗ oᵗ srcᵗ) | Right S<:Sᵗ = result₀ (largest Fᶠ (λ o → o)) where

    data LargestSrc (G : Type) : Set where

      yes : ∀ S₀ T₀ →

        Overloads F (S₀ ⇒ T₀) →
        T₀ <: T →
        (∀ {S′ T′} → Overloads G (S′ ⇒ T′) → T′ <: T → (S′ <: S₀)) →
        -----------------------
        LargestSrc G

      no : ∀ S₀ T₀ →

        Overloads F (S₀ ⇒ T₀) →
        T₀ ≮: T →  
        (∀ {S′ T′} → Overloads G (S′ ⇒ T′) → T₀ <: T′) →
        -----------------------
        LargestSrc G

    largest : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → LargestSrc G
    largest {S′ ⇒ T′} (S′ⁿ ⇒ T′ⁿ) G⊆F with dec-subtypingⁿ  T′ⁿ Tⁿ
    largest {S′ ⇒ T′} (S′ⁿ ⇒ T′ⁿ) G⊆F | Left T′≮:T = no S′ T′ (G⊆F here) T′≮:T λ { here → <:-refl }
    largest {S′ ⇒ T′} (S′ⁿ ⇒ T′ⁿ) G⊆F | Right T′<:T = yes S′ T′ (G⊆F here) T′<:T (λ { here _ → <:-refl })
    largest (Gᶠ ∩ Hᶠ) GH⊆F with largest Gᶠ (GH⊆F ∘ left) | largest Hᶠ (GH⊆F ∘ right)
    largest (Gᶠ ∩ Hᶠ) GH⊆F | no S₁ T₁ o₁ T₁≮:T tgt₁ | no S₂ T₂ o₂ T₂≮:T tgt₂ with sat-∩ o₁ o₂
    largest (Gᶠ ∩ Hᶠ) GH⊆F | no S₁ T₁ o₁ T₁≮:T tgt₁ | no S₂ T₂ o₂ T₂≮:T tgt₂ | defn o src tgt with dec-subtypingⁿ (normal-overload-tgt Fᶠ o) Tⁿ
    largest (Gᶠ ∩ Hᶠ) GH⊆F | no S₁ T₁ o₁ T₁≮:T tgt₁ | no S₂ T₂ o₂ T₂≮:T tgt₂ | defn o src tgt | Left T₀≮:T = no _ _ o T₀≮:T (λ { (left o) → <:-trans tgt (<:-trans <:-∩-left (tgt₁ o)) ; (right o) → <:-trans tgt (<:-trans <:-∩-right (tgt₂ o)) })
    largest (Gᶠ ∩ Hᶠ) GH⊆F | no S₁ T₁ o₁ T₁≮:T tgt₁ | no S₂ T₂ o₂ T₂≮:T tgt₂ | defn o src tgt | Right T₀<:T = yes _ _ o T₀<:T (λ { (left o) p → CONTRADICTION (<:-impl-¬≮: p (<:-trans-≮: (tgt₁ o) T₁≮:T)) ; (right o) p → CONTRADICTION (<:-impl-¬≮: p (<:-trans-≮: (tgt₂ o) T₂≮:T)) })
    largest (Gᶠ ∩ Hᶠ) GH⊆F | no S₁ T₁ o₁ T₁≮:T tgt₁ | yes S₂ T₂ o₂ T₂<:T src₂ = yes S₂ T₂ o₂ T₂<:T (λ { (left o) p → CONTRADICTION (<:-impl-¬≮: p (<:-trans-≮: (tgt₁ o) T₁≮:T)) ; (right o) p → src₂ o p })
    largest (Gᶠ ∩ Hᶠ) GH⊆F | yes S₁ T₁ o₁ T₁<:T src₁ | no S₂ T₂ o₂ T₂≮:T tgt₂ = yes S₁ T₁ o₁ T₁<:T (λ { (left o) p → src₁ o p ; (right o) p → CONTRADICTION (<:-impl-¬≮: p (<:-trans-≮: (tgt₂ o) T₂≮:T)) })
    largest (Gᶠ ∩ Hᶠ) GH⊆F | yes S₁ T₁ o₁ T₁<:T src₁ | yes S₂ T₂ o₂ T₂<:T src₂ with sat-∪ o₁ o₂
    largest (Gᶠ ∩ Hᶠ) GH⊆F | yes S₁ T₁ o₁ T₁<:T src₁ | yes S₂ T₂ o₂ T₂<:T src₂ | defn o src tgt = yes _ _ o (<:-trans tgt (<:-∪-lub T₁<:T T₂<:T))
      (λ { (left o) T′<:T → <:-trans (src₁ o T′<:T) (<:-trans <:-∪-left src)
         ; (right o) T′<:T → <:-trans (src₂ o T′<:T) (<:-trans <:-∪-right src)
         })

    result₀ : LargestSrc F → Either (F ≮: (S ⇒ T)) (F <:ᵒ (S ⇒ T))
    result₀ (no S₀ T₀ o₀ (witness t T₀t ¬Tt) tgt₀) = Left (witness (function-tgt t) (ov-language Fᶠ (λ o → function-tgt (tgt₀ o t T₀t))) (function-tgt ¬Tt))
    result₀ (yes S₀ T₀ o₀ T₀<:T src₀) with dec-subtypingⁿ Sⁿ (normal-overload-src Fᶠ o₀)
    result₀ (yes S₀ T₀ o₀ T₀<:T src₀) | Right S<:S₀ = Right λ { here → defn o₀ S<:S₀ T₀<:T }
    result₀ (yes S₀ T₀ o₀ T₀<:T src₀) | Left (witness s Ss ¬S₀s) = Left (result₁ (smallest Fᶠ (λ o → o))) where

      data SmallestTgt (G : Type) : Set where

        defn : ∀ S₁ T₁ →

          Overloads F (S₁ ⇒ T₁) →
          Language S₁ s →
          (∀ {S′ T′} → Overloads G (S′ ⇒ T′) → Language S′ s → (T₁ <: T′)) →
          -----------------------
          SmallestTgt G

      smallest : ∀ {G} → (FunType G) → (G ⊆ᵒ F) → SmallestTgt G
      smallest {S′ ⇒ T′} _ G⊆F with dec-language S′ s
      smallest {S′ ⇒ T′} _ G⊆F | Left ¬S′s = defn Sᵗ Tᵗ oᵗ (S<:Sᵗ s Ss) λ { here S′s → CONTRADICTION (language-comp s ¬S′s S′s) }
      smallest {S′ ⇒ T′} _ G⊆F | Right S′s = defn S′ T′ (G⊆F here) S′s (λ { here _ → <:-refl })
      smallest (Gᶠ ∩ Hᶠ) GH⊆F with smallest Gᶠ (GH⊆F ∘ left) | smallest Hᶠ (GH⊆F ∘ right)
      smallest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ R₁s tgt₁ | defn S₂ T₂ o₂ R₂s tgt₂ with sat-∩ o₁ o₂
      smallest (Gᶠ ∩ Hᶠ) GH⊆F | defn S₁ T₁ o₁ R₁s tgt₁ | defn S₂ T₂ o₂ R₂s tgt₂ | defn o src tgt = defn _ _ o (src s (R₁s , R₂s))
        (λ { (left o) S′s → <:-trans (<:-trans tgt <:-∩-left) (tgt₁ o S′s)
           ; (right o) S′s → <:-trans (<:-trans tgt <:-∩-right) (tgt₂ o S′s)
           })

      result₁ : SmallestTgt F → (F ≮: (S ⇒ T))
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) with dec-subtypingⁿ (normal-overload-tgt Fᶠ o₁) Tⁿ
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) | Right T₁<:T = CONTRADICTION (language-comp s ¬S₀s (src₀ o₁ T₁<:T s S₁s))
      result₁ (defn S₁ T₁ o₁ S₁s tgt₁) | Left (witness t T₁t ¬Tt) = witness (function-ok s t) (ov-language Fᶠ lemma) (function-ok Ss ¬Tt) where

        lemma : ∀ {S′ T′} → Overloads F (S′ ⇒ T′) → Language (S′ ⇒ T′) (function-ok s t)
        lemma {S′} o with dec-language S′ s
        lemma {S′} o | Left ¬S′s = function-ok₁ ¬S′s
        lemma {S′} o | Right S′s = function-ok₂ (tgt₁ o S′s t T₁t)

dec-subtypingˢᶠ F Fˢ (G ∩ H) with dec-subtypingˢᶠ F Fˢ G | dec-subtypingˢᶠ F Fˢ H
dec-subtypingˢᶠ F Fˢ (G ∩ H) | Left F≮:G | _ = Left (≮:-∩-left F≮:G)
dec-subtypingˢᶠ F Fˢ (G ∩ H) | _ | Left F≮:H = Left (≮:-∩-right F≮:H)
dec-subtypingˢᶠ F Fˢ (G ∩ H) | Right F<:G | Right F<:H = Right (λ { (left o) → F<:G o ; (right o) → F<:H o })

dec-subtypingᶠ F G with dec-subtypingˢᶠ (normal-saturate F) (saturated F) G
dec-subtypingᶠ F G | Left H≮:G = Left (<:-trans-≮: (saturate-<: F) H≮:G)
dec-subtypingᶠ F G | Right H<:G = Right (<:-trans (<:-saturate F) (<:ᵒ-impl-<: (normal-saturate F) G H<:G))

dec-subtypingᶠⁿ T never = Left (witness function (fun-function T) never)
dec-subtypingᶠⁿ T unknown = Right <:-unknown
dec-subtypingᶠⁿ T (U ⇒ V) = dec-subtypingᶠ T (U ⇒ V)
dec-subtypingᶠⁿ T (U ∩ V) = dec-subtypingᶠ T (U ∩ V)
dec-subtypingᶠⁿ T (U ∪ V) with dec-subtypingᶠⁿ T U
dec-subtypingᶠⁿ T (U ∪ V) | Left (witness t p q) = Left (witness t p (q , fun-¬scalar V T p))
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

-- As a corollary, for saturated functions
-- <:ᵒ coincides with <:, that is F is a subtype of (S ⇒ T) precisely
-- when one of its overloads is.

<:-impl-<:ᵒ : ∀ {F G} → FunType F → Saturated F → FunType G → (F <: G) → (F <:ᵒ G)
<:-impl-<:ᵒ {F} {G} Fᶠ Fˢ Gᶠ F<:G with dec-subtypingˢᶠ Fᶠ Fˢ Gᶠ
<:-impl-<:ᵒ {F} {G} Fᶠ Fˢ Gᶠ F<:G | Left F≮:G = CONTRADICTION (<:-impl-¬≮: F<:G F≮:G)
<:-impl-<:ᵒ {F} {G} Fᶠ Fˢ Gᶠ F<:G | Right F<:ᵒG = F<:ᵒG
