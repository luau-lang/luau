{-# OPTIONS --rewriting #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; Object; function_is_end; defn; alloc; ok; next; lookup-not-allocated) renaming (_≡_⊕_↦_ to _≡ᴴ_⊕_↦_; _[_] to _[_]ᴴ; ∅ to ∅ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; Warningᴼ; Warningᴴᴱ; Warningᴴᴮ; UnallocatedAddress; UnboundVariable; app₀; app₁; app₂; block; return; local₀; local₁; local₂; function₀; function₁; function₂; heap; expr; addr)
open import Luau.Substitution using (_[_/_]ᴮ; _[_/_]ᴱ; _[_/_]ᴮunless_; var_[_/_]ᴱwhenever_)
open import Luau.Syntax using (Expr; yes; var; var_∈_; _⟨_⟩∈_; _$_; addr; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg)
open import Luau.Type using (Type; strict; nil; _⇒_; bot; tgt)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∈_; _⊢ᴱ_∈_; nil; var; addr; app; function; block; done; return; local; orBot)
open import Luau.Value using (val; nil; addr)
open import Luau.Var using (_≡ⱽ_)
open import Luau.Addr using (_≡ᴬ_)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import Luau.VarCtxt using (VarCtxt; ∅)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (_≢_; sym; cong; trans; subst₁)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.TypeCheck(strict) using (typeOfᴼ; typeOfᴹᴼ; typeOfⱽ; typeOfᴱ; typeOfᴮ; typeOfᴱⱽ; typeCheckᴱ; typeCheckᴮ; typeCheckᴼ; typeCheckᴴᴱ; typeCheckᴴᴮ)
open import Luau.OpSem using (_⊢_⟶*_⊣_; _⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app; function; beta; return; block; done; local; subst; refl; step)
open import Luau.RuntimeError using (RuntimeErrorᴱ; RuntimeErrorᴮ; NilIsNotAFunction; UnboundVariable; SEGV; app; block; local; return)

src = Luau.Type.src strict

_≡ᵀ_ : ∀ (T U : Type) → Dec(T ≡ U)
_≡ᵀ_ = {!!}

_≡ᴹᵀ_ : ∀ (T U : Maybe Type) → Dec(T ≡ U)
_≡ᴹᵀ_ = {!!}

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

reflect-substitutionᴱ : ∀ {H Γ Γ′ T} M v x → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴱ H (typeCheckᴱ H Γ (M [ v / x ]ᴱ)) → Warningᴱ H (typeCheckᴱ H Γ′ M)
reflect-substitutionᴱ-whenever-yes : ∀ {H Γ Γ′ T} v x y (p : x ≡ y) → (typeOfᴱ H Γ (val v) ≡ T) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever yes p)) → Warningᴱ H (typeCheckᴱ H Γ′ (var y))
reflect-substitutionᴱ-whenever-no : ∀ {H Γ Γ′ T} v x y (p : x ≢ y) → (typeOfᴱ H Γ (val v) ≡ T) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever no p)) → Warningᴱ H (typeCheckᴱ H Γ′ (var y))
reflect-substitutionᴮ : ∀ {H Γ Γ′ T} B v x → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮ)) → Warningᴮ H (typeCheckᴮ H Γ′ B)
reflect-substitutionᴮ-unless-yes : ∀ {H Γ Γ′ T} B v x y (r : x ≡ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless yes r)) → Warningᴮ H (typeCheckᴮ H Γ′ B)

reflect-substitutionᴱ (var y) v x p q W with x ≡ⱽ y
reflect-substitutionᴱ (var y) v x p q W | yes r = {!!} -- reflect-substitutionᴱ-whenever-yes v x y r (typeOfᴱⱽ v) p q W
reflect-substitutionᴱ (var y) v x p q W | no r = {!!} -- reflect-substitutionᴱ-whenever-no v x y r (typeOfᴱⱽ v) p q W
reflect-substitutionᴱ (addr a) v x p q (UnallocatedAddress a r) = UnallocatedAddress a r
reflect-substitutionᴱ (M $ N) v x p q (app₀ r) = {!!}
reflect-substitutionᴱ (M $ N) v x p q (app₁ W) = app₁ (reflect-substitutionᴱ M v x p q W)
reflect-substitutionᴱ (M $ N) v x p q (app₂ W) = app₂ (reflect-substitutionᴱ N v x p q W)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p q (function₀ f r) = {!!}
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p refl (function₁ f W) with (x ≡ⱽ y)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p refl (function₁ f W) | yes r = function₁ f (reflect-substitutionᴮ-unless-yes B v x y r p (⊕-overwrite r) W)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p refl (function₁ f W) | no r = function₁ f (reflect-substitutionᴮ B v x p (⊕-swap r) W)
reflect-substitutionᴱ (block b is B end) v x p q (block b W) = block b (reflect-substitutionᴮ B v x p q W)

reflect-substitutionᴱ-whenever-no v x y r refl refl (UnboundVariable y p) = UnboundVariable y {!!}
reflect-substitutionᴱ-whenever-yes (addr a) x x refl refl refl (UnallocatedAddress a p) = {!!}

reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p q (function₀ f r) = {!!}
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p q (function₁ f W) with (x ≡ⱽ y)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p refl (function₁ f W) | yes r = function₁ f (reflect-substitutionᴮ-unless-yes C v x y r p (⊕-overwrite r) W)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p refl (function₁ f W) | no r = function₁ f (reflect-substitutionᴮ C v x p (⊕-swap r) W)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p q (function₂ f W) with (x ≡ⱽ f)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p refl (function₂ f W)| yes r = function₂ f (reflect-substitutionᴮ-unless-yes B v x f r p (⊕-overwrite r) W)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p refl (function₂ f W)| no r = function₂ f (reflect-substitutionᴮ B v x p (⊕-swap r) W)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p q (local₀ r) = {!!}
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p q (local₁ W) = local₁ (reflect-substitutionᴱ M v x p q W)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p q (local₂ W) with (x ≡ⱽ y)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p refl (local₂ W) | yes r = local₂ (reflect-substitutionᴮ-unless-yes B v x y r p (⊕-overwrite r) W)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p refl (local₂ W) | no r = local₂ (reflect-substitutionᴮ B v x p (⊕-swap r) W)
reflect-substitutionᴮ (return M ∙ B) v x p q (return W) = return (reflect-substitutionᴱ M v x p q W)

reflect-substitutionᴮ-unless-yes B v x y r p refl W = W

reflect-weakeningᴱ : ∀ {H H′ Γ M} → (H ⊑ H′) → Warningᴱ H′ (typeCheckᴱ H′ Γ M) → Warningᴱ H (typeCheckᴱ H Γ M)
reflect-weakeningᴮ : ∀ {H H′ Γ B} → (H ⊑ H′) → Warningᴮ H′ (typeCheckᴮ H′ Γ B) → Warningᴮ H (typeCheckᴮ H Γ B)

reflect-weakeningᴱ h (UnboundVariable x p) = (UnboundVariable x p)
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
reflect-weakeningᴮ h (function₀ f p) with heap-weakeningᴮ h
reflect-weakeningᴮ h (function₀ f p) | ok q = function₀ f (λ r → p (trans r q))
reflect-weakeningᴮ h (function₀ f p) | warning W = function₁ f W
reflect-weakeningᴮ h (function₁ f W) = function₁ f (reflect-weakeningᴮ h W)
reflect-weakeningᴮ h (function₂ f W) = function₂ f (reflect-weakeningᴮ h W)

reflect-weakeningᴼ : ∀ {H H′ O} → (H ⊑ H′) → Warningᴼ H′ (typeCheckᴼ H′ O) → Warningᴼ H (typeCheckᴼ H O)
reflect-weakeningᴼ h (function₀ f p) with heap-weakeningᴮ h
reflect-weakeningᴼ h (function₀ f p) | ok q = function₀ f (λ r → p (trans r q))
reflect-weakeningᴼ h (function₀ f p) | warning W = function₁ f W
reflect-weakeningᴼ h (function₁ f W′) = function₁ f (reflect-weakeningᴮ h W′)

reflectᴱ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴱ H′ (typeCheckᴱ H′ ∅ M′) → Warningᴴᴱ H (typeCheckᴴᴱ H ∅ M)
reflectᴮ : ∀ {H H′ B B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴮ H′ (typeCheckᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)

reflectᴱ (app s) (app₀ p) with preservationᴱ s | heap-weakeningᴱ (redn-⊑ s)
reflectᴱ (app s) (app₀ p) | ok q | ok q′ = expr (app₀ (λ r → p (trans (trans (cong src (sym q)) r) q′)))
reflectᴱ (app s) (app₀ p) | warning W | _ = expr (app₁ W)
reflectᴱ (app s) (app₀ p) | _ | warning W  = expr (app₂ W)
reflectᴱ (app s) (app₁ W′) with reflectᴱ s W′
reflectᴱ (app s) (app₁ W′) | heap W = heap W
reflectᴱ (app s) (app₁ W′) | expr W = expr (app₁ W)
reflectᴱ (app s) (app₂ W′) = expr (app₂ (reflect-weakeningᴱ (redn-⊑ s) W′))
reflectᴱ (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} q) (block f (local₀ p)) = expr (app₀ (λ r → p (trans (cong src (cong orBot (cong typeOfᴹᴼ (sym q)))) r)))
reflectᴱ (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} q) (block f (local₁ W)) = expr (app₂ W)
reflectᴱ (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} q) (block f (local₂ W)) = heap (addr a q (function₁ f W))
reflectᴱ (block s) (block b W′) with reflectᴮ s W′
reflectᴱ (block s) (block b W′) | heap W = heap W
reflectᴱ (block s) (block b W′) | block W = expr (block b W)
reflectᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) (UnallocatedAddress a ())
reflectᴱ (return q) W = expr (block _ (return W))

reflectᴮ (local s) (local₀ p) with preservationᴱ s
reflectᴮ (local s) (local₀ p) | ok q = block (local₀ (λ r → p (trans r q)))
reflectᴮ (local s) (local₀ p) | warning W = block (local₁ W)
reflectᴮ (local s) (local₁ W′) with reflectᴱ s W′ -- local₁ (reflectᴱ s W)
reflectᴮ (local s) (local₁ W′) | heap W = heap W
reflectᴮ (local s) (local₁ W′) | expr W = block (local₁ W)
reflectᴮ (local s) (local₂ W′) = block (local₂ (reflect-weakeningᴮ (redn-⊑ s) W′))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = v}) W with just T ≡ᴹᵀ typeOfⱽ H v
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = v}) W | yes p = block (local₂ (reflect-substitutionᴮ _ v x p refl W))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = nil}) W | no p = block (local₀ λ r → p (cong just r))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = addr a}) W | no p with remember(H [ a ]ᴴ)
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = addr a}) W | no p | (nothing , q) = block (local₁ (UnallocatedAddress a q))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = addr a}) W | no p | (just O , q) = block (local₀ (λ r → p (trans (cong just (trans r (cong orBot (cong typeOfᴹᴼ q)))) (cong typeOfᴹᴼ (sym q)))))
reflectᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) W = block (function₂ f (reflect-weakeningᴮ (snoc refl defn) (reflect-substitutionᴮ _ _ f refl refl W)))
reflectᴮ (return s) (return W′) with reflectᴱ s W′
reflectᴮ (return s) (return W′) | heap W = heap W
reflectᴮ (return s) (return W′) | expr W = block (return W)

reflectᴴᴱ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴴᴱ H′ (typeCheckᴴᴱ H′ ∅ M′) → Warningᴴᴱ H (typeCheckᴴᴱ H ∅ M)
reflectᴴᴮ : ∀ {H H′ B B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)

reflectᴴᴱ s (expr W′) = reflectᴱ s W′
reflectᴴᴱ (function {a = a} p) (heap (addr b refl W′)) with b ≡ᴬ a
reflectᴴᴱ (function defn) (heap (addr a refl (function₀ f p))) | yes refl with heap-weakeningᴮ (snoc refl defn)
reflectᴴᴱ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | ok r = expr (function₀ f λ q → p (trans q r))
reflectᴴᴱ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | warning W = expr (function₁ f W)
reflectᴴᴱ (function defn) (heap (addr a refl (function₁ f W′))) | yes refl = expr (function₁ f (reflect-weakeningᴮ (snoc refl defn) W′))
reflectᴴᴱ (function p) (heap (addr b refl W′)) | no r = heap (addr b (lookup-not-allocated p r) (reflect-weakeningᴼ (snoc refl p) W′))
reflectᴴᴱ (app s) (heap W′) with reflectᴴᴱ s (heap W′)
reflectᴴᴱ (app s) (heap W′) | heap W = heap W
reflectᴴᴱ (app s) (heap W′) | expr W = expr (app₁ W)
reflectᴴᴱ (beta p) (heap W′) = heap W′
reflectᴴᴱ (block s) (heap W′) with reflectᴴᴮ s (heap W′)
reflectᴴᴱ (block s) (heap W′) | heap W = heap W
reflectᴴᴱ (block s) (heap W′) | block W = expr (block _ W)
reflectᴴᴱ (return s) (heap W′) = heap W′
reflectᴴᴱ done (heap W′) = heap W′

reflectᴴᴮ s (block W′) = reflectᴮ s W′
reflectᴴᴮ (local {x = var x ∈ T} s) (heap W′) with reflectᴴᴱ s (heap W′)
reflectᴴᴮ (local {x = var x ∈ T} s) (heap W′) | heap W = heap W
reflectᴴᴮ (local {x = var x ∈ T} s) (heap W′) | expr W = block (local₁ W)
reflectᴴᴮ (subst) (heap W′) = heap W′
reflectᴴᴮ (function {a = a} p) (heap (addr b refl W′)) with b ≡ᴬ a
reflectᴴᴮ (function defn) (heap (addr a refl (function₀ f p))) | yes refl with heap-weakeningᴮ (snoc refl defn)
reflectᴴᴮ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | ok r = block (function₀ f (λ q → p (trans q r)))
reflectᴴᴮ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | warning W = block (function₁ f W)
reflectᴴᴮ (function defn) (heap (addr a refl (function₁ f W′))) | yes refl = block (function₁ f (reflect-weakeningᴮ (snoc refl defn) W′))
reflectᴴᴮ (function p) (heap (addr b refl W′)) | no r = heap (addr b (lookup-not-allocated p r) (reflect-weakeningᴼ (snoc refl p) W′))
reflectᴴᴮ (return s) (heap W′) with reflectᴴᴱ s (heap W′)
reflectᴴᴮ (return s) (heap W′) | heap W = heap W
reflectᴴᴮ (return s) (heap W′) | expr W = block (return W)

reflect* : ∀ {H H′ B B′} → (H ⊢ B ⟶* B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)
reflect* refl W = W
reflect* (step s t) W = reflectᴴᴮ s (reflect* t W)

bot-not-obj : ∀ O → bot ≢ typeOfᴼ O
bot-not-obj (function f ⟨ var x ∈ T ⟩∈ U is B end) ()

runtimeWarningᴱ : ∀ {H M} → RuntimeErrorᴱ H M → Warningᴱ H (typeCheckᴱ H ∅ M)
runtimeWarningᴮ : ∀ {H B} → RuntimeErrorᴮ H B → Warningᴮ H (typeCheckᴮ H ∅ B)

runtimeWarningᴱ (NilIsNotAFunction {V = nil}) = (app₀ (λ ()))
runtimeWarningᴱ {H} (NilIsNotAFunction {addr a}) with remember (H [ a ]ᴴ)
runtimeWarningᴱ (NilIsNotAFunction {addr a}) | (nothing , p) = app₂ (UnallocatedAddress a p)
runtimeWarningᴱ (NilIsNotAFunction {addr a}) | (just O , p) = app₀ λ r → bot-not-obj O (trans r (cong orBot (cong typeOfᴹᴼ p)))
runtimeWarningᴱ (UnboundVariable x) = UnboundVariable x refl
runtimeWarningᴱ (SEGV a p) = UnallocatedAddress a p
runtimeWarningᴱ (app err) = app₁ (runtimeWarningᴱ err)
runtimeWarningᴱ (block b err) = block b (runtimeWarningᴮ err)

runtimeWarningᴮ (local (var x ∈ T) err) = local₁ (runtimeWarningᴱ err)
runtimeWarningᴮ (return err) = return (runtimeWarningᴱ err)

wellTypedProgramsDontGoWrong : ∀ {H′ B B′} → (∅ᴴ ⊢ B ⟶* B′ ⊣ H′) → (RuntimeErrorᴮ H′ B′) → Warningᴮ ∅ᴴ (typeCheckᴮ ∅ᴴ ∅ B)
wellTypedProgramsDontGoWrong t err with reflect* t (block (runtimeWarningᴮ err))
wellTypedProgramsDontGoWrong t err | heap (addr a refl ())
wellTypedProgramsDontGoWrong t err | block W = W
