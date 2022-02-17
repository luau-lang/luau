{-# OPTIONS --rewriting #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; HeapValue; function_is_end; defn; alloc; ok; next) renaming (_≡_⊕_↦_ to _≡ᴴ_⊕_↦_; _[_] to _[_]ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; BadlyTypedFunctionAddress; UnallocatedAddress; app₀; app₁; app₂; block; return; local₀; local₁; local₂; function₀; function₁; function₂)
open import Luau.Substitution using (_[_/_]ᴮ; _[_/_]ᴱ; _[_/_]ᴮunless_; var_[_/_]ᴱwhenever_)
open import Luau.Syntax using (Expr; yes; var; var_∈_; _⟨_⟩∈_; _$_; addr; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg)
open import Luau.Type using (Type; strict; nil; _⇒_; bot; tgt)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∈_; _⊢ᴱ_∈_; nil; var; addr; app; function; block; done; return; local)
open import Luau.Value using (val; nil; addr)
open import Luau.Var using (_≡ⱽ_)
open import Luau.Addr using (_≡ᴬ_)
open import Luau.AddrCtxt using (AddrCtxt)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import Luau.VarCtxt using (VarCtxt; ∅)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (_≢_; sym; cong; trans; subst₁)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.TypeCheck(strict) using (declaredTypeᴴ; typeOfⱽ; typeOfᴴ; typeOfᴱ; typeOfᴮ; typeOfᴱⱽ; typeCheckᴱ; typeCheckᴮ)
open import Luau.OpSem using (_⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app; function; beta; return; block; done; local; subst)

src = Luau.Type.src strict

_≡ᵀ_ : ∀ (T U : Type) → Dec(T ≡ U)
_≡ᵀ_ = {!!}

data _⊑_ (H : Heap yes) : Heap yes → Set where
  refl : (H ⊑ H)
  snoc : ∀ {H′ H″ a V} → (H ⊑ H′) → (H″ ≡ᴴ H′ ⊕ a ↦ V) → (H ⊑ H″)

warning-⊑ : ∀ {H H′ Γ T M} {D : Γ ⊢ᴱ M ∈ T} → (H ⊑ H′) → (Warningᴱ H′ D) → Warningᴱ H D
warning-⊑ = {!!}

data LookupResult (H : Heap yes) a V : Set where
  just : (H [ a ]ᴴ ≡ just V) → LookupResult H a V
  nothing : (H [ a ]ᴴ ≡ nothing) → LookupResult H a V

lookup-⊑-just : ∀ {H H′ V} a → (H ⊑ H′) → (H′ [ a ]ᴴ ≡ just V) → LookupResult H a V
lookup-⊑-just = {!!}

lookup-⊑-nothing : ∀ {H H′} a → (H ⊑ H′) → (H′ [ a ]ᴴ ≡ nothing) → (H [ a ]ᴴ ≡ nothing)
lookup-⊑-nothing = {!!}

data OrWarningᴱ {Γ M T} (H : Heap yes) (D : Γ ⊢ᴱ M ∈ T) A : Set where
  ok : A → OrWarningᴱ H D A
  warning : Warningᴱ H D → OrWarningᴱ H D A

data OrWarningᴮ {Γ B T} (H : Heap yes) (D : Γ ⊢ᴮ B ∈ T) A : Set where
  ok : A → OrWarningᴮ H D A
  warning : Warningᴮ H D → OrWarningᴮ H D A

redn-⊑ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → (H ⊑ H′)
redn-⊑ = {!!}

⊕-overwrite : ∀ {Γ x y T U} → (x ≡ y) → ((Γ ⊕ x ↦ T) ⊕ y ↦ U) ≡ (Γ ⊕ y ↦ U)
⊕-overwrite = {!!}

⊕-swap : ∀ {Γ x y T U} → (x ≢ y) → ((Γ ⊕ x ↦ T) ⊕ y ↦ U) ≡ ((Γ ⊕ y ↦ U) ⊕ x ↦ T)
⊕-swap = {!!}

substitutivityᴱ : ∀ {Γ T H M v x} → (T ≡ typeOfᴱ H Γ (val v)) → (typeOfᴱ H (Γ ⊕ x ↦ T) M ≡ typeOfᴱ H Γ (M [ v / x ]ᴱ))
substitutivityᴮ : ∀ {Γ T H B v x} → (T ≡ typeOfᴱ H Γ (val v)) → (typeOfᴮ H (Γ ⊕ x ↦ T) B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮ))

substitutivityᴱ = {!!}
substitutivityᴮ = {!!}

heap-weakeningᴱ : ∀ {H H′ M Γ} → (H ⊑ H′) → OrWarningᴱ H (typeCheckᴱ H Γ M) (typeOfᴱ H Γ M ≡ typeOfᴱ H′ Γ M)
heap-weakeningᴮ : ∀ {H H′ B Γ} → (H ⊑ H′) → OrWarningᴮ H (typeCheckᴮ H Γ B) (typeOfᴮ H Γ B ≡ typeOfᴮ H′ Γ B)

heap-weakeningᴱ = {!!}
heap-weakeningᴮ = {!!}

preservationᴱ : ∀ {H H′ M M′ Γ} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → OrWarningᴱ H (typeCheckᴱ H Γ M) (typeOfᴱ H Γ M ≡ typeOfᴱ H′ Γ M′)
preservationᴮ : ∀ {H H′ B B′ Γ} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → OrWarningᴮ H (typeCheckᴮ H Γ B) (typeOfᴮ H Γ B ≡ typeOfᴮ H′ Γ B′)

preservationᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) = ok refl
preservationᴱ (app s) with preservationᴱ s
preservationᴱ (app s) | ok p = ok (cong tgt p)
preservationᴱ (app s) | warning W = warning (app₁ W)
preservationᴱ (beta {F = f ⟨ var x ∈ S ⟩∈ T} p) = {!!} -- ok (trans (cong tgt (cong typeOfᴴ p)) {!!})
preservationᴱ (block s) with preservationᴮ s
preservationᴱ (block s) | ok p = ok p
preservationᴱ (block {b = b} s) | warning W = warning (block b W)
preservationᴱ (return p) = ok refl
preservationᴱ done = ok refl

preservationᴮ (local {x = var x ∈ T} s) with heap-weakeningᴮ (redn-⊑ s)
preservationᴮ (local {x = var x ∈ T} s) | ok p = ok p
preservationᴮ (local {x = var x ∈ T} s) | warning W = warning (local₂ W)
preservationᴮ (subst {x = var x ∈ T} {B = B}) = ok (substitutivityᴮ {B = B} {!!})
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) with heap-weakeningᴮ (snoc refl defn)
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) | ok r = ok (trans r (substitutivityᴮ {T = S ⇒ T} {B = B} refl))
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) | warning W = warning (function₂ f W)
preservationᴮ (return s) with preservationᴱ s
preservationᴮ (return s) | ok p = ok p
preservationᴮ (return s) | warning W = warning (return W)

reflect-substitutionᴱ : ∀ {H Γ Γ′ T} M v x → (T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴱ H (typeCheckᴱ H Γ (M [ v / x ]ᴱ)) → Warningᴱ H (typeCheckᴱ H Γ′ M)
reflect-substitutionᴱ-whenever-yes : ∀ {H Γ Γ′ T} v x y (p : x ≡ y) → (typeOfᴱ H Γ (val v) ≡ T) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever yes p)) → Warningᴱ H (typeCheckᴱ H Γ′ (var y))
reflect-substitutionᴱ-whenever-no : ∀ {H Γ Γ′ T} v x y (p : x ≢ y) → (typeOfᴱ H Γ (val v) ≡ T) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever no p)) → Warningᴱ H (typeCheckᴱ H Γ′ (var y))
reflect-substitutionᴮ : ∀ {H Γ Γ′ T} B v x → (T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮ)) → Warningᴮ H (typeCheckᴮ H Γ′ B)
reflect-substitutionᴮ-unless-yes : ∀ {H Γ Γ′ T} B v x y (r : x ≡ y) → (T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless yes r)) → Warningᴮ H (typeCheckᴮ H Γ′ B)

reflect-substitutionᴱ (var y) v x refl q W with x ≡ⱽ y
reflect-substitutionᴱ (var y) v x refl q W | yes r = reflect-substitutionᴱ-whenever-yes v x y r (typeOfᴱⱽ v) q W
reflect-substitutionᴱ (var y) v x refl q W | no r = reflect-substitutionᴱ-whenever-no v x y r (typeOfᴱⱽ v) q W
reflect-substitutionᴱ (addr a) v x p q (BadlyTypedFunctionAddress a f r W) = BadlyTypedFunctionAddress a f r W
reflect-substitutionᴱ (addr a) v x p q (UnallocatedAddress a r) = UnallocatedAddress a r
reflect-substitutionᴱ (M $ N) v x p q (app₀ r) = {!!}
reflect-substitutionᴱ (M $ N) v x p q (app₁ W) = app₁ (reflect-substitutionᴱ M v x p q W)
reflect-substitutionᴱ (M $ N) v x p q (app₂ W) = app₂ (reflect-substitutionᴱ N v x p q W)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p q (function₀ f r) = {!!}
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p refl (function₁ f W) with (x ≡ⱽ y)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p refl (function₁ f W) | yes r = function₁ f (reflect-substitutionᴮ-unless-yes B v x y r p (⊕-overwrite r) W)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p refl (function₁ f W) | no r = function₁ f (reflect-substitutionᴮ B v x p (⊕-swap r) W)
reflect-substitutionᴱ (block b is B end) v x p q (block b W) = block b (reflect-substitutionᴮ B v x p q W)

reflect-substitutionᴱ-whenever-no v x y r refl refl ()
reflect-substitutionᴱ-whenever-yes (addr a) x x refl refl refl (BadlyTypedFunctionAddress a f p W) = {!!}
reflect-substitutionᴱ-whenever-yes (addr a) x x refl refl refl (UnallocatedAddress a p) = {!!}

reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p q (function₁ f W) = {!!}
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p q (function₂ f W) = {!!}
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p q (local₀ r) = {!!}
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p q (local₁ W) = local₁ (reflect-substitutionᴱ M v x p q W)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p q (local₂ W) = {!!}
reflect-substitutionᴮ (return M ∙ B) v x p q (return W) = return (reflect-substitutionᴱ M v x p q W)

reflect-substitutionᴮ-unless-yes B v x y r p refl W = W

reflect-weakeningᴱ : ∀ {H H′ Γ M} → (H ⊑ H′) → Warningᴱ H′ (typeCheckᴱ H′ Γ M) → Warningᴱ H (typeCheckᴱ H Γ M)
reflect-weakeningᴮ : ∀ {H H′ Γ B} → (H ⊑ H′) → Warningᴮ H′ (typeCheckᴮ H′ Γ B) → Warningᴮ H (typeCheckᴮ H Γ B)

reflect-weakeningᴱ h (BadlyTypedFunctionAddress a f p W) with lookup-⊑-just a h p
reflect-weakeningᴱ h (BadlyTypedFunctionAddress a f p W) | just q = BadlyTypedFunctionAddress a f q (reflect-weakeningᴮ h W)
reflect-weakeningᴱ h (BadlyTypedFunctionAddress a f p W) | nothing q = UnallocatedAddress a q
reflect-weakeningᴱ h (UnallocatedAddress a p) = UnallocatedAddress a (lookup-⊑-nothing a h p)
reflect-weakeningᴱ h (app₀ p) with heap-weakeningᴱ h |  heap-weakeningᴱ h
reflect-weakeningᴱ h (app₀ p) | ok q₁ | ok q₂ = app₀ (λ r → p (trans (cong src (sym q₁)) (trans r q₂)))
reflect-weakeningᴱ h (app₀ p) | warning W | _ = app₁ W
reflect-weakeningᴱ h (app₀ p) | _ | warning W = app₂ W
reflect-weakeningᴱ h (app₁ W) = app₁ (reflect-weakeningᴱ h W)
reflect-weakeningᴱ h (app₂ W) = app₂ (reflect-weakeningᴱ h W)
reflect-weakeningᴱ h (function₀ f p) with heap-weakeningᴮ h
reflect-weakeningᴱ h (function₀ f p) | ok q = function₀ f (λ r → p (trans r q))
reflect-weakeningᴱ h (function₀ f p) | warning W = function₁ f W
reflect-weakeningᴱ h (function₁ f W) = function₁ f (reflect-weakeningᴮ h W)
reflect-weakeningᴱ h (block b W) = block b (reflect-weakeningᴮ h W)

reflect-weakeningᴮ h (return W) = return (reflect-weakeningᴱ h W)
reflect-weakeningᴮ h (local₀ p) with heap-weakeningᴱ h
reflect-weakeningᴮ h (local₀ p) | ok q = local₀ (λ r → p (trans r q))
reflect-weakeningᴮ h (local₀ p) | warning W = local₁ W
reflect-weakeningᴮ h (local₁ W) = local₁ (reflect-weakeningᴱ h W)
reflect-weakeningᴮ h (local₂ W) = local₂ (reflect-weakeningᴮ h W)
reflect-weakeningᴮ h (function₁ f W) = function₁ f (reflect-weakeningᴮ h W)
reflect-weakeningᴮ h (function₂ f W) = function₂ f (reflect-weakeningᴮ h W)

reflectᴱ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴱ H′ (typeCheckᴱ H′ ∅ M′) → Warningᴱ H (typeCheckᴱ H ∅ M)
reflectᴮ : ∀ {H H′ B B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴮ H′ (typeCheckᴮ H′ ∅ B′) → Warningᴮ H (typeCheckᴮ H ∅ B)

reflectᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) (BadlyTypedFunctionAddress a f refl W) = function₁ f (reflect-weakeningᴮ (snoc refl defn) W)
reflectᴱ (app s) (app₀ p) with preservationᴱ s | heap-weakeningᴱ (redn-⊑ s)
reflectᴱ (app s) (app₀ p) | ok q | ok q′ = app₀ (λ r → p (trans (trans (cong src (sym q)) r) q′))
reflectᴱ (app s) (app₀ p) | warning W | _ = app₁ W
reflectᴱ (app s) (app₀ p) | _ | warning W  = app₂ W
reflectᴱ (app s) (app₁ W) = app₁ (reflectᴱ s W)
reflectᴱ (app s) (app₂ W) = app₂ (reflect-weakeningᴱ (redn-⊑ s) W)
reflectᴱ (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} q) (block f (local₀ p)) = app₀ (λ r → p (trans (sym (cong src (cong declaredTypeᴴ q))) r))
reflectᴱ (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} q) (block f (local₁ W)) = app₂ W
reflectᴱ (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} q) (block f (local₂ {T = T′} W)) = app₁ (BadlyTypedFunctionAddress a f q W)
reflectᴱ (block s) (block b W)  = block b (reflectᴮ s W)
reflectᴱ (return q) W = block _ (return W)

reflectᴮ (local s) (local₀ p) with preservationᴱ s
reflectᴮ (local s) (local₀ p) | ok q = local₀ (λ r → p (trans r q))
reflectᴮ (local s) (local₀ p) | warning W = local₁ W
reflectᴮ (local s) (local₁ W) = local₁ (reflectᴱ s W)
reflectᴮ (local s) (local₂ W) = local₂ (reflect-weakeningᴮ (redn-⊑ s) W)
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = v}) W with T ≡ᵀ (typeOfᴱ H ∅ (val v))
reflectᴮ (subst {x = var x ∈ T} {v = v}) W | yes refl = local₂ (reflect-substitutionᴮ _ v x (typeOfᴱⱽ v) refl W)
reflectᴮ (subst {x = var x ∈ T} {v = v}) W | no p = local₀ p
reflectᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) W = function₂ f (reflect-weakeningᴮ (snoc refl defn) (reflect-substitutionᴮ _ _ f refl refl W))
reflectᴮ (return s) (return W) = return (reflectᴱ s W)

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
