{-# OPTIONS --rewriting #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; HeapValue; function_is_end; defn; alloc; ok; next; lookup-next) renaming (_≡_⊕_↦_ to _≡ᴴ_⊕_↦_; _[_] to _[_]ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; bot; disagree; addr; app₁; app₂; block; return; local₁)
open import Luau.Substitution using (_[_/_]ᴮ; _[_/_]ᴱ)
open import Luau.Syntax using (Expr; yes; var_∈_; _⟨_⟩∈_; _$_; addr; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg)
open import Luau.Type using (Type; strict; nil; _⇒_; bot; tgt)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∋_∈_⊣_; _⊢ᴱ_∋_∈_⊣_; nil; var; addr; app; function; block; done; return; local)
open import Luau.Value using (val; nil; addr)
open import Luau.Addr using (_≡ᴬ_)
open import Luau.AddrCtxt using (AddrCtxt)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_; ∅-[]) renaming (_[_] to _[_]ⱽ)
open import Luau.VarCtxt using (VarCtxt; ∅)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (sym; cong; trans; subst₁)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.TypeCheck(strict) using (typeOfᴴ; typeOfᴱ; typeOfᴮ; typeCheckᴱ; typeCheckᴮ)
open import Luau.OpSem using (_⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app; function; beta; return; block; done; local; subst)

{-# REWRITE lookup-next #-}

src = Luau.Type.src strict

data _⊑_ (H : Heap yes) : Heap yes → Set where
  refl : (H ⊑ H)
  snoc : ∀ {H′ H″ a V} → (H ⊑ H′) → (H″ ≡ᴴ H′ ⊕ a ↦ V) → (H ⊑ H″)

warning-⊑ : ∀ {H H′ Γ Δ S T M} {D : Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ} → (H ⊑ H′) → (Warningᴱ H′ D) → Warningᴱ H D
warning-⊑ = {!!}

data TypeOfᴱ-⊑-Result H H′ Γ M : Set where
  ok : (typeOfᴱ H Γ M ≡ typeOfᴱ H′ Γ M) → TypeOfᴱ-⊑-Result H H′ Γ M
  warning : (∀ {S} → Warningᴱ H (typeCheckᴱ H Γ S M)) → TypeOfᴱ-⊑-Result H H′ Γ M

data TypeOfᴮ-⊑-Result H H′ Γ B : Set where
  ok : (typeOfᴮ H Γ B ≡ typeOfᴮ H′ Γ B) → TypeOfᴮ-⊑-Result H H′ Γ B
  warning : (∀ {S} → Warningᴮ H (typeCheckᴮ H Γ S B)) → TypeOfᴮ-⊑-Result H H′ Γ B

typeOfᴱ-⊑ : ∀ {H H′ Γ M} → (H ⊑ H′) → (TypeOfᴱ-⊑-Result H H′ Γ M)
typeOfᴱ-⊑ = {!!}

typeOfᴮ-⊑ : ∀ {H H′ Γ B} → (H ⊑ H′) → (TypeOfᴮ-⊑-Result H H′ Γ B)
typeOfᴮ-⊑ = {!!}

blah : ∀ {H H′ Γ S S′ M} →  (H ⊑ H′) → (S ≡ S′) → (Warningᴱ H′ (typeCheckᴱ H′ Γ S′ M)) → (Warningᴱ H (typeCheckᴱ H Γ S M))
blah = {!!}

bloz : ∀ {H Γ S S′ M} → (S ≡ S′) → (Warningᴱ H (typeCheckᴱ H Γ S′ M)) → (Warningᴱ H (typeCheckᴱ H Γ S M))
bloz = {!!}

redn-⊑ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → (H ⊑ H′)
redn-⊑ = {!!}

substitutivityᴱ : ∀ {Γ T H M v x} → (T ≡ typeOfᴱ H Γ (val v)) → (typeOfᴱ H (Γ ⊕ x ↦ T) M ≡ typeOfᴱ H Γ (M [ v / x ]ᴱ))
substitutivityᴮ : ∀ {Γ T H B v x} → (T ≡ typeOfᴱ H Γ (val v)) → (typeOfᴮ H (Γ ⊕ x ↦ T) B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮ))

substitutivityᴱ = {!!}
substitutivityᴮ = {!!}

preservationᴱ : ∀ {H H′ M M′ Γ} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → (typeOfᴱ H Γ M ≡ typeOfᴱ H′ Γ M′)
preservationᴮ : ∀ {H H′ B B′ Γ} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → (typeOfᴮ H Γ B ≡ typeOfᴮ H′ Γ B′)

preservationᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) = refl
preservationᴱ (app s) = cong tgt (preservationᴱ s)
preservationᴱ (beta {F = f ⟨ var x ∈ S ⟩∈ T} p) = trans (cong tgt (cong typeOfᴴ p)) {!!}
preservationᴱ (block s) = preservationᴮ s
preservationᴱ (return p) = refl
preservationᴱ done = refl

preservationᴮ (local {x = var x ∈ T} {B = B} s) with typeOfᴮ-⊑ {B = B} (redn-⊑ s)
preservationᴮ (local {x = var x ∈ T} s) | ok p = p
preservationᴮ (local {x = var x ∈ T} s) | warning W = {!!}
preservationᴮ (subst {x = var x ∈ T} {B = B}) = substitutivityᴮ {B = B} {!!}
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) with typeOfᴮ-⊑ {B = B} (snoc refl defn)
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) | ok r = trans r (substitutivityᴮ {T = S ⇒ T} {B = B} refl)
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) | warning W = {!!}
preservationᴮ (return s) = preservationᴱ s

reflectᴱ : ∀ {H H′ M M′ S} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴱ H′ (typeCheckᴱ H′ ∅ S M′) → Warningᴱ H (typeCheckᴱ H ∅ S M)
reflectᴮ : ∀ {H H′ B B′ S} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴮ H′ (typeCheckᴮ H′ ∅ S B′) → Warningᴮ H (typeCheckᴮ H ∅ S B)

reflectᴱ s W with redn-⊑ s
reflectᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) (addr a _ r) | p  = CONTRADICTION (r refl)
reflectᴱ (app s) (bot x) | p  = {!x!}
reflectᴱ (app s) (app₁ W) | p with typeOfᴱ-⊑ p
reflectᴱ (app s) (app₁ W) | p | ok q = app₁ (bloz (cong (λ ∙ → ∙ ⇒ _) q) (reflectᴱ s W))
reflectᴱ (app s) (app₁ W) | p | warning W′ = app₂ W′
reflectᴱ (app s) (app₂ W) | p = app₂ (blah p (cong src (preservationᴱ s)) W)
reflectᴱ (beta s) (bot x₁) | p = {!!}
reflectᴱ (beta {F = f ⟨ var x ∈ T ⟩∈ U} q) (block _ (disagree x₁)) | p = {!!}
reflectᴱ (beta {F = f ⟨ var x ∈ T ⟩∈ U} q) (block _ (local₁ W)) | p = app₂ (bloz (cong src (cong typeOfᴴ q)) W)
reflectᴱ (block s) (bot x₁) | p = {!!}
reflectᴱ (block s) (block b W) | p = block b (reflectᴮ s W)
reflectᴱ (return q) W | p  = block _ (return W)
reflectᴱ done (bot x) | p = {!!}

reflectᴮ s = {!!}

-- reflectᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) (bot ())
-- reflectᴱ (function defn) (addr a T q) = CONTRADICTION (q refl)
-- reflectᴱ (app s) (bot x) = {!x!}
-- reflectᴱ (app s) (app₁ W) = app₁ {!reflectᴱ s W!}
-- reflectᴱ (app s) (app₂ W) = {!!}
-- reflectᴱ (beta x) W = {!!}
-- reflectᴱ (block x) W = {!!}
-- reflectᴱ (return x) W = {!!}
-- reflectᴱ done W = {!!}

-- heap-miss : ∀ {Σ HV T} → (Σ ▷ HV ∈ T) → (HV ≡ nothing) → (T ≡ bot)
-- heap-miss nothing refl = refl

-- data ProgressResultᴱ {Σ Γ S M T Δ} (H : Heap yes) (D : Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) : Set
-- data ProgressResultᴮ {Σ Γ S B T Δ} (H : Heap yes) (D : Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ) : Set

-- data ProgressResultᴱ {Σ Γ S M T Δ} H D where

--   value : ∀ V → (M ≡ val V) → ProgressResultᴱ H D
--   warning : (Warningᴱ Σ D) → ProgressResultᴱ H D
--   step : ∀ {M′ H′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → ProgressResultᴱ H D

-- data ProgressResultᴮ {Σ Γ S B T Δ} H D where

--   done : (B ≡ done) → ProgressResultᴮ H D
--   return : ∀ V {C} → (B ≡ (return (val V) ∙ C)) → ProgressResultᴮ H D
--   warning : (Warningᴮ Σ D) → ProgressResultᴮ H D
--   step : ∀ {B′ H′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → ProgressResultᴮ H D

-- progressᴱ : ∀ {Σ Γ S M T Δ} H → (Σ ▷ H ✓) → (D : Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) → (Γ ≡ ∅) → ProgressResultᴱ H D
-- progressᴮ : ∀ {Σ Γ S B T Δ} H → (Σ ▷ H ✓) → (D : Σ ▷ Γ ⊢ᴮ S ∋ B ∈ T ⊣ Δ) → (Γ ≡ ∅) → ProgressResultᴮ H D

-- progressᴱ H h nil _ = value nil refl
-- progressᴱ H h (var x p) refl = warning (bot p)
-- progressᴱ H h (addr a refl) _ = value (addr a) refl
-- progressᴱ H h (app D₁ D₂) p with progressᴱ H h D₁ p
-- progressᴱ H h (app nil D₂) p | value nil refl = warning (bot refl)
-- progressᴱ H h (app (var _ _) D₂) p | value nil ()
-- progressᴱ H h (app (app _ _) D₂) p | value nil ()
-- progressᴱ H h (app (function _) D₂) p | value nil ()
-- progressᴱ H h (app (block _ _) D₂) p | value nil ()
-- progressᴱ H h (app (addr _ refl) D₂) p | value (addr a) refl with remember(H [ a ]ᴴ)
-- progressᴱ H h (app (addr _ refl) D₂) p | value (addr a) refl | (nothing , r) = warning (bot (cong tgt (heap-miss (h a) r)))
-- progressᴱ H h (app (addr _ refl) D₂) p | value (addr a) refl | (just(function f ⟨ var x ∈ S ⟩∈ T is B end) , r) = step (beta r)
-- progressᴱ H h (app D₁ D₂) p | warning W = warning (app₁ W)
-- progressᴱ H h (app D₁ D₂) p | step S = step (app S)
-- progressᴱ H h (function D) _ with alloc H _
-- progressᴱ H h (function D) _ | ok a H′ r = step (function r)
-- progressᴱ H h (block b D) q with progressᴮ H h D q
-- progressᴱ H h (block b D) q | done refl = step done
-- progressᴱ H h (block b D) q | return V refl = step (return refl)
-- progressᴱ H h (block b D) q | warning W = warning (block b W)
-- progressᴱ H h (block b D) q | step S = step (block S)

-- progressᴮ H h done q = done refl
-- progressᴮ H h (return D₁ D₂) q with progressᴱ H h D₁ q
-- progressᴮ H h (return D₁ D₂) q | value V refl = return V refl
-- progressᴮ H h (return D₁ D₂) q | warning W = warning (return W)
-- progressᴮ H h (return D₁ D₂) q | step S = step (return S)
-- progressᴮ H h (local D₁ D₂) q with progressᴱ H h D₁ q
-- progressᴮ H h (local D₁ D₂) q | value V refl = step subst
-- progressᴮ H h (local D₁ D₂) q | warning W = warning (local₁ W)
-- progressᴮ H h (local D₁ D₂) q | step S = step (local S)
-- progressᴮ H h (function D₁ D₂) q with alloc H _
-- progressᴮ H h (function D₁ D₂) q | ok a H′ r = step (function r)

import FFI.Data.Aeson
{-# REWRITE FFI.Data.Aeson.singleton-insert-empty #-}

_≡ᵀ_ : (T U : Type) → Dec (T ≡ U)
_≡ᵀ_ = {!!}

-- data LookupResult {Σ V S} (D : Σ ▷ V ∈ S) : Set where

--   function : ∀ f {x B T U W} →
--     (S ≡ (T ⇒ U)) →
--     (V ≡ just(function f ⟨ var x ∈ T ⟩∈ U is B end)) →
--     (Σ ▷ (x ↦ T) ⊢ᴮ U ∋ B ∈ U ⊣ (x ↦ W)) →
--     LookupResult D

--   warningᴴ :
--     Warningᴴ(D) →
--     LookupResult D

-- lookup : ∀ {Σ V T} (D : Σ ▷ V ∈ T) → LookupResult D
-- lookup nothing = warningᴴ nothing
-- lookup (function f {U = U} {V = V} D) with U ≡ᵀ V
-- lookup (function f D) | yes refl = function f refl refl D
-- lookup (function f D) | no p = warningᴴ (function f (disagree p))

-- data PreservationResultᴮ {Σ S Δ T H B} (D : ∅ ⊢ᴮ S ∋ B ∈ T ⊣ Δ) M′ H′ : Set where

--   ok : ∀ {Δ′} → (∅ ⊢ᴮ S ∋ M′ ∈ T ⊣ Δ′) → PreservationResultᴮ D M′ H′
--   warning : Warningᴮ Σ D → PreservationResultᴮ D M′ H′

-- data PreservationResultᴱ {Σ S Δ T M} (D : ∅ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) M′ : Set where

--   ok : ∀ {Δ′} → (∅ ⊢ᴱ S ∋ M′ ∈ T ⊣ Δ′) → PreservationResultᴱ D M′ H′
--   warning : Warningᴱ Σ D → PreservationResultᴱ D M′ H′

-- preservationᴱ : ∀ {Σ S Δ T H H′ M M′} → (D : ∅ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) → (s : H ⊢ M ⟶ᴱ M′ ⊣ H′) → PreservationResultᴱ D M′ H′
-- preservationᴱ {S = S} {T = T} h D s with S ≡ᵀ T
-- preservationᴱ h D s | no p = warning (disagree p)
-- preservationᴱ h (app D₁ D₂) (app s) | yes refl with preservationᴱ h D₁ s
-- preservationᴱ h (app D₁ D₂) (app s) | yes refl | ok h′ D₁′ = ok h′ (app D₁′ {!D₂!})
-- preservationᴱ h (app D₁ D₂) (app s) | yes refl | warning W = warning (app₁ W)
-- -- preservationᴱ h (app (addr a p) D₂) (beta q) | yes refl with lookup (h a)
-- -- preservationᴱ h (app (addr a p) D₂) (beta q) | yes refl | function f r₁ r₂ D₁ with trans p r₁ | trans (sym q) r₂
-- -- preservationᴱ h (app (addr a p) D₂) (beta q) | yes refl | function f r₁ r₂ D₁ | refl | refl = ok h (block f (local D₂ D₁))
-- -- preservationᴱ h (app (addr a p) D₂) (beta q) | yes refl | warningᴴ W = warningᴴ a W

-- -- preservationᴱ h (app {T = T} {U = U} (addr a p) D₂) (beta q) with subst₂ (λ X Y → _ ▷ X ∈ Y) q (sym p) (h a)
-- -- preservationᴱ {S = S} h (app {T = T} {U = U} (addr a p) D₂) (beta q) | function f {T = T′} {U = U′} {V = V′} D with S ≡ᵀ U′ | U′ ≡ᵀ V′
-- -- preservationᴱ h (app {T = T} {U = U} (addr a p) D₂) (beta q) | function f {T = T′} {U = U′} {V = V′} D | yes refl | yes refl = ok h (block _ (local D₂ D))
-- -- preservationᴱ h (app {T = T} {U = U} (addr a p) D₂) (beta q) | function f {T = T′} {U = U′} D | no r | _ = warningᴱ (disagree r)
-- -- preservationᴱ h (app {T = T} {U = U} (addr a p) D₂) (beta q) | function f {T = T′} {U = U′} D | yes refl | no r = warningᴴ a {!function f (disagree r)!} -- (subst₁ Warningᴴ {!!} {!(function f (disagree r))!}) -- (function f (disagree r))

-- -- with src T ≡ᵀ U --  = ok h (block f (local {!D₂!} {!!}))
-- -- preservationᴱ h (app {T = T} {U = U} D₁ D₂) (beta {F = f ⟨ var x ∈ _ ⟩∈ R} p) | yes refl = ok h (block f (local D₂ {!!})) -- with src T ≡ᵀ S --  = ok h (block f (local {!D₂!} {!!}))
-- -- preservationᴱ h (app {T = T} {U = U} D₁ D₂) (beta {F = f ⟨ var x ∈ S ⟩∈ R} p) | no q = warning (app₀ {!q!}) -- with src T ≡ᵀ S --  = ok h (block f (local {!D₂!} {!!}))
-- preservationᴱ h D s | yes refl = {!!}
-- preservationᴱ h (function D) (function p) = {!x!}
-- preservationᴱ h (block b D) (block s) = {!!}
-- preservationᴱ h (block b D) (return p) = {!x!}
-- preservationᴱ h (block b D) done = {!!}
