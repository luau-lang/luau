{-# OPTIONS --rewriting #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; Object; function_is_end; defn; alloc; ok; next; lookup-not-allocated) renaming (_≡_⊕_↦_ to _≡ᴴ_⊕_↦_; _[_] to _[_]ᴴ; ∅ to ∅ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; Warningᴼ; Warningᴴᴱ; Warningᴴᴮ; UnallocatedAddress; UnboundVariable; app₀; app₁; app₂; block; return; local₀; local₁; local₂; function₀; function₁; function₂; heap; expr; addr)
open import Luau.Substitution using (_[_/_]ᴮ; _[_/_]ᴱ; _[_/_]ᴮunless_; var_[_/_]ᴱwhenever_)
open import Luau.Syntax using (Expr; yes; var; var_∈_; _⟨_⟩∈_; _$_; addr; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg)
open import Luau.Type using (Type; strict; nil; _⇒_; bot; tgt; _≡ᵀ_; _≡ᴹᵀ_)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∈_; _⊢ᴱ_∈_; _⊢ᴴᴮ_▷_∈_; _⊢ᴴᴱ_▷_∈_; nil; var; addr; app; function; block; done; return; local; orBot)
open import Luau.Value using (val; nil; addr)
open import Luau.Var using (_≡ⱽ_)
open import Luau.Addr using (_≡ᴬ_)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_; ⊕-lookup-miss; ⊕-swap; ⊕-over) renaming (_[_] to _[_]ⱽ)
open import Luau.VarCtxt using (VarCtxt; ∅)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (_≢_; sym; cong; trans; subst₁)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.TypeCheck(strict) using (typeOfᴼ; typeOfᴹᴼ; typeOfⱽ; typeOfᴱ; typeOfᴮ; typeOfᴱⱽ; typeCheckᴱ; typeCheckᴮ; typeCheckᴼ; typeCheckᴴᴱ; typeCheckᴴᴮ)
open import Luau.OpSem using (_⊢_⟶*_⊣_; _⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app₁; app₂; function; beta; return; block; done; local; subst; refl; step)
open import Luau.RuntimeError using (RuntimeErrorᴱ; RuntimeErrorᴮ; NilIsNotAFunction; UnboundVariable; SEGV; app₁; app₂; block; local; return)

src = Luau.Type.src strict

data _⊑_ (H : Heap yes) : Heap yes → Set where
  refl : (H ⊑ H)
  snoc : ∀ {H′ a V} → (H′ ≡ᴴ H ⊕ a ↦ V) → (H ⊑ H′)

rednᴱ⊑ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → (H ⊑ H′)
rednᴮ⊑ : ∀ {H H′ B B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → (H ⊑ H′)

rednᴱ⊑ (function p) = snoc p
rednᴱ⊑ (app₁ s) = rednᴱ⊑ s
rednᴱ⊑ (app₂ p s) = rednᴱ⊑ s
rednᴱ⊑ (beta p q) = refl
rednᴱ⊑ (block s) = rednᴮ⊑ s
rednᴱ⊑ (return p) = refl
rednᴱ⊑ done = refl

rednᴮ⊑ (local s) = rednᴱ⊑ s
rednᴮ⊑ subst = refl
rednᴮ⊑ (function p) = snoc p
rednᴮ⊑ (return s) = rednᴱ⊑ s

data LookupResult (H : Heap yes) a V : Set where
  just : (H [ a ]ᴴ ≡ just V) → LookupResult H a V
  nothing : (H [ a ]ᴴ ≡ nothing) → LookupResult H a V

lookup-⊑-nothing : ∀ {H H′} a → (H ⊑ H′) → (H′ [ a ]ᴴ ≡ nothing) → (H [ a ]ᴴ ≡ nothing)
lookup-⊑-nothing {H} a refl p = p
lookup-⊑-nothing {H} a (snoc defn) p with a ≡ᴬ next H 
lookup-⊑-nothing {H} a (snoc defn) p | yes refl = refl
lookup-⊑-nothing {H} a (snoc o) p | no q = trans (lookup-not-allocated o q) p

data OrWarningᴱ {Γ M T} (H : Heap yes) (D : Γ ⊢ᴱ M ∈ T) A : Set where
  ok : A → OrWarningᴱ H D A
  warning : Warningᴱ H D → OrWarningᴱ H D A

data OrWarningᴮ {Γ B T} (H : Heap yes) (D : Γ ⊢ᴮ B ∈ T) A : Set where
  ok : A → OrWarningᴮ H D A
  warning : Warningᴮ H D → OrWarningᴮ H D A

data OrWarningᴴᴱ {Γ M T} H (D : Γ ⊢ᴴᴱ H ▷ M ∈ T) A : Set where
  ok : A → OrWarningᴴᴱ H D A
  warning : Warningᴴᴱ H D → OrWarningᴴᴱ H D A

data OrWarningᴴᴮ {Γ B T} H (D : Γ ⊢ᴴᴮ H ▷ B ∈ T) A : Set where
  ok : A → OrWarningᴴᴮ H D A
  warning : Warningᴴᴮ H D → OrWarningᴴᴮ H D A

heap-weakeningᴱ : ∀ {H H′ M Γ} → (H ⊑ H′) → OrWarningᴱ H (typeCheckᴱ H Γ M) (typeOfᴱ H Γ M ≡ typeOfᴱ H′ Γ M)
heap-weakeningᴮ : ∀ {H H′ B Γ} → (H ⊑ H′) → OrWarningᴮ H (typeCheckᴮ H Γ B) (typeOfᴮ H Γ B ≡ typeOfᴮ H′ Γ B)

heap-weakeningᴱ {M = nil} h = ok refl
heap-weakeningᴱ {M = var x} h = ok refl
heap-weakeningᴱ {M = addr a} refl = ok refl
heap-weakeningᴱ {M = addr a} (snoc {a = b} defn) with a ≡ᴬ b
heap-weakeningᴱ {M = addr a} (snoc {a = a} defn) | yes refl = warning (UnallocatedAddress a refl)
heap-weakeningᴱ {M = addr a} (snoc {a = b} p) | no q = ok (cong orBot (cong typeOfᴹᴼ (lookup-not-allocated p q)))
heap-weakeningᴱ {M = M $ N} h with heap-weakeningᴱ h
heap-weakeningᴱ {M = M $ N} h | ok p = ok (cong tgt p)
heap-weakeningᴱ {M = M $ N} h | warning W = warning (app₁ W)
heap-weakeningᴱ {M = function f ⟨ var x ∈ T ⟩∈ U is B end} h = ok refl
heap-weakeningᴱ {M = block b is B end} h with heap-weakeningᴮ h
heap-weakeningᴱ {M = block b is B end} h | ok p = ok p
heap-weakeningᴱ {M = block b is B end} h | warning W = warning (block b W)
heap-weakeningᴮ {B = function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B} h with heap-weakeningᴮ h
heap-weakeningᴮ {B = function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B} h | ok p = ok p
heap-weakeningᴮ {B = function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B} h | warning W = warning (function₂ f W)
heap-weakeningᴮ {B = local var x ∈ T ← M ∙ B} h with heap-weakeningᴮ h
heap-weakeningᴮ {B = local var x ∈ T ← M ∙ B} h | ok p = ok p
heap-weakeningᴮ {B = local var x ∈ T ← M ∙ B} h | warning W = warning (local₂ W)
heap-weakeningᴮ {B = return M ∙ B} h with heap-weakeningᴱ h
heap-weakeningᴮ {B = return M ∙ B} h | ok p = ok p
heap-weakeningᴮ {B = return M ∙ B} h | warning W = warning (return W)
heap-weakeningᴮ {B = done} h = ok refl

bot-not-obj : ∀ O → bot ≢ typeOfᴼ O
bot-not-obj (function f ⟨ var x ∈ T ⟩∈ U is B end) ()

typeOf-val-not-bot : ∀ {H Γ} v → OrWarningᴱ H (typeCheckᴱ H Γ (val v)) (bot ≢ typeOfᴱ H Γ (val v))
typeOf-val-not-bot nil = ok (λ ())
typeOf-val-not-bot {H = H} (addr a) with remember (H [ a ]ᴴ)
typeOf-val-not-bot {H = H} (addr a) | (just O , p) = ok (λ q → bot-not-obj O (trans q (cong orBot (cong typeOfᴹᴼ p))))
typeOf-val-not-bot {H = H} (addr a) | (nothing , p) = warning (UnallocatedAddress a p)

substitutivityᴱ : ∀ {Γ T H} M v x → (just T ≡ typeOfⱽ H v) → (typeOfᴱ H (Γ ⊕ x ↦ T) M ≡ typeOfᴱ H Γ (M [ v / x ]ᴱ))
substitutivityᴱ-whenever-yes : ∀ {Γ T H} v x y (p : x ≡ y) → (just T ≡ typeOfⱽ H v) → (typeOfᴱ H (Γ ⊕ x ↦ T) (var y) ≡ typeOfᴱ H Γ (var y [ v / x ]ᴱwhenever (yes p)))
substitutivityᴱ-whenever-no : ∀ {Γ T H} v x y (p : x ≢ y) → (just T ≡ typeOfⱽ H v) → (typeOfᴱ H (Γ ⊕ x ↦ T) (var y) ≡ typeOfᴱ H Γ (var y [ v / x ]ᴱwhenever (no p)))
substitutivityᴮ : ∀ {Γ T H} B v x → (just T ≡ typeOfⱽ H v) → (typeOfᴮ H (Γ ⊕ x ↦ T) B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮ))
substitutivityᴮ-unless-yes : ∀ {Γ Γ′ T H} B v x y (p : x ≡ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ) → (typeOfᴮ H Γ′ B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮunless (yes p)))
substitutivityᴮ-unless-no : ∀ {Γ Γ′ T H} B v x y (p : x ≢ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → (typeOfᴮ H Γ′ B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮunless (no p)))

substitutivityᴱ nil v x p = refl
substitutivityᴱ (var y) v x p with x ≡ⱽ y
substitutivityᴱ (var y) v x p | yes q = substitutivityᴱ-whenever-yes v x y q p
substitutivityᴱ (var y) v x p | no q = substitutivityᴱ-whenever-no v x y q p
substitutivityᴱ (addr a) v x p = refl
substitutivityᴱ (M $ N) v x p = cong tgt (substitutivityᴱ M v x p)
substitutivityᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p = refl
substitutivityᴱ (block b is B end) v x p = substitutivityᴮ B v x p
substitutivityᴱ-whenever-yes v x x refl q = trans (cong orBot q) (sym (typeOfᴱⱽ v))
substitutivityᴱ-whenever-no v x y p q = cong orBot ( sym (⊕-lookup-miss x y _ _ p))
substitutivityᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p with x ≡ⱽ f
substitutivityᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p | yes q = substitutivityᴮ-unless-yes B v x f q p (⊕-over q)
substitutivityᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p | no q = substitutivityᴮ-unless-no B v x f q p (⊕-swap q)
substitutivityᴮ (local var y ∈ T ← M ∙ B) v x p with x ≡ⱽ y
substitutivityᴮ (local var y ∈ T  ← M ∙ B) v x p | yes q = substitutivityᴮ-unless-yes B v x y q p (⊕-over q)
substitutivityᴮ (local var y ∈ T  ← M ∙ B) v x p | no q =  substitutivityᴮ-unless-no B v x y q p (⊕-swap q)
substitutivityᴮ (return M ∙ B) v x p = substitutivityᴱ M v x p
substitutivityᴮ done v x p = refl
substitutivityᴮ-unless-yes B v x x refl q refl = refl
substitutivityᴮ-unless-no B v x y p q refl = substitutivityᴮ B v x q

preservationᴱ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → OrWarningᴴᴱ H (typeCheckᴴᴱ H ∅ M) (typeOfᴱ H ∅ M ≡ typeOfᴱ H′ ∅ M′)
preservationᴮ : ∀ {H H′ B B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → OrWarningᴴᴮ H (typeCheckᴴᴮ H ∅ B) (typeOfᴮ H ∅ B ≡ typeOfᴮ H′ ∅ B′)

preservationᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) = ok refl
preservationᴱ (app₁ s) with preservationᴱ s
preservationᴱ (app₁ s) | ok p = ok (cong tgt p)
preservationᴱ (app₁ s) | warning (expr W) = warning (expr (app₁ W))
preservationᴱ (app₁ s) | warning (heap W) = warning (heap W)
preservationᴱ (app₂ p s) with heap-weakeningᴱ (rednᴱ⊑ s)
preservationᴱ (app₂ p s) | ok q = ok (cong tgt q)
preservationᴱ (app₂ p s) | warning W  = warning (expr (app₁ W))
preservationᴱ {H = H} (beta {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) with remember (typeOfⱽ H V)
preservationᴱ {H = H} (beta {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) | (just U , q) with S ≡ᵀ U | T ≡ᵀ typeOfᴮ H (x ↦ S) B
preservationᴱ {H = H} (beta {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) | (just U , q) | yes refl | yes refl = ok (trans (cong tgt (cong orBot (cong typeOfᴹᴼ p))) (substitutivityᴮ B V x (sym q)))
preservationᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) | (just U , q) | yes refl | no r = warning (heap (addr a p (function₀ f r)))
preservationᴱ {H = H} (beta {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) | (just U , q) | no r | _ = warning (expr (app₀ (λ s → r (trans (trans (sym (cong src (cong orBot (cong typeOfᴹᴼ p)))) (trans s (typeOfᴱⱽ V))) (cong orBot q)))))
preservationᴱ {H = H} (beta {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) | (nothing , q) with typeOf-val-not-bot V
preservationᴱ {H = H} (beta {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) | (nothing , q) | ok r = CONTRADICTION (r (sym (trans (typeOfᴱⱽ V) (cong orBot q))))
preservationᴱ {H = H} (beta {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} {V = V} p refl) | (nothing , q) | warning W = warning (expr (app₂ W))
preservationᴱ (block s) with preservationᴮ s
preservationᴱ (block s) | ok p = ok p
preservationᴱ (block {b = b} s) | warning (block W) = warning (expr (block b W))
preservationᴱ (block {b = b} s) | warning (heap W) = warning (heap W)
preservationᴱ (return p) = ok refl
preservationᴱ done = ok refl

preservationᴮ (local {x = var x ∈ T} s) with heap-weakeningᴮ (rednᴱ⊑ s)
preservationᴮ (local {x = var x ∈ T} s) | ok p = ok p
preservationᴮ (local {x = var x ∈ T} s) | warning W = warning (block (local₂ W))
preservationᴮ {H = H} (subst {v = v}) with remember (typeOfⱽ H v)
preservationᴮ (subst {x = var x ∈ T} {v = v} {B = B}) | (just U , p) with T ≡ᵀ U
preservationᴮ (subst {x = var x ∈ T} {v = v} {B = B}) | (just T , p) | yes refl = ok (substitutivityᴮ B v x (sym p))
preservationᴮ (subst {x = var x ∈ T} {v = v} {B = B}) | (just U , p) | no q = warning (block (local₀ (λ r → q (trans r (trans (typeOfᴱⱽ v) (cong orBot p))))))
preservationᴮ (subst {x = var x ∈ T} {v = v}) | (nothing , p) with typeOf-val-not-bot v
preservationᴮ (subst {x = var x ∈ T} {v = v}) | (nothing , p) | ok q = CONTRADICTION (q (sym (trans (typeOfᴱⱽ v) (cong orBot p))))
preservationᴮ (subst {x = var x ∈ T} {v = v}) | (nothing , p) | warning W = warning (block (local₁ W))
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) with heap-weakeningᴮ (snoc defn)
preservationᴮ (function {a = a} {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) | ok r = ok (trans r (substitutivityᴮ {T = S ⇒ T} B (addr a) f refl))
preservationᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} {B = B} defn) | warning W = warning (block (function₂ f W))
preservationᴮ (return s) with preservationᴱ s
preservationᴮ (return s) | ok p = ok p
preservationᴮ (return s) | warning (expr W) = warning (block (return W))
preservationᴮ (return s) | warning (heap W) = warning (heap W)

reflect-substitutionᴱ : ∀ {H Γ T} M v x → (just T ≡ typeOfⱽ H v) → Warningᴱ H (typeCheckᴱ H Γ (M [ v / x ]ᴱ)) → Warningᴱ H (typeCheckᴱ H (Γ ⊕ x ↦ T) M)
reflect-substitutionᴱ-whenever-yes : ∀ {H Γ T} v x y (p : x ≡ y) → (just T ≡ typeOfⱽ H v) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever yes p)) → Warningᴱ H (typeCheckᴱ H (Γ ⊕ x ↦ T) (var y))
reflect-substitutionᴱ-whenever-no : ∀ {H Γ T} v x y (p : x ≢ y) → (just T ≡ typeOfⱽ H v) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever no p)) → Warningᴱ H (typeCheckᴱ H  (Γ ⊕ x ↦ T) (var y))
reflect-substitutionᴮ : ∀ {H Γ T} B v x → (just T ≡ typeOfⱽ H v) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮ)) → Warningᴮ H (typeCheckᴮ H (Γ ⊕ x ↦ T) B)
reflect-substitutionᴮ-unless-yes : ∀ {H Γ Γ′ T} B v x y (r : x ≡ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless yes r)) → Warningᴮ H (typeCheckᴮ H Γ′ B)
reflect-substitutionᴮ-unless-no : ∀ {H Γ Γ′ T} B v x y (r : x ≢ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless no r)) → Warningᴮ H (typeCheckᴮ H Γ′ B)

reflect-substitutionᴱ (var y) v x p W with x ≡ⱽ y
reflect-substitutionᴱ (var y) v x p W | yes r = reflect-substitutionᴱ-whenever-yes v x y r p W
reflect-substitutionᴱ (var y) v x p W | no r = reflect-substitutionᴱ-whenever-no v x y r p W
reflect-substitutionᴱ (addr a) v x p (UnallocatedAddress a r) = UnallocatedAddress a r
reflect-substitutionᴱ (M $ N) v x p (app₀ q) = app₀ (λ s → q (trans (cong src (sym (substitutivityᴱ M v x p))) (trans s (substitutivityᴱ N v x p))))
reflect-substitutionᴱ (M $ N) v x p (app₁ W) = app₁ (reflect-substitutionᴱ M v x p W)
reflect-substitutionᴱ (M $ N) v x p (app₂ W) = app₂ (reflect-substitutionᴱ N v x p W)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₀ f q) with (x ≡ⱽ y)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₀ f q) | yes r = function₀ f (λ s → q (trans s (substitutivityᴮ-unless-yes B v x y r p (⊕-over r))))
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₀ f q) | no r = function₀ f (λ s → q (trans s (substitutivityᴮ-unless-no B v x y r p (⊕-swap r))))
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₁ f W) with (x ≡ⱽ y)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₁ f W) | yes r = function₁ f (reflect-substitutionᴮ-unless-yes B v x y r p (⊕-over r) W)
reflect-substitutionᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₁ f W) | no r = function₁ f (reflect-substitutionᴮ-unless-no B v x y r p (⊕-swap r) W)
reflect-substitutionᴱ (block b is B end) v x p (block b W) = block b (reflect-substitutionᴮ B v x p W)

reflect-substitutionᴱ-whenever-no v x y p q (UnboundVariable y r) = UnboundVariable y (trans (sym (⊕-lookup-miss x y _ _ p)) r)
reflect-substitutionᴱ-whenever-yes (addr a) x x refl p (UnallocatedAddress a q) with trans p (cong typeOfᴹᴼ q)
reflect-substitutionᴱ-whenever-yes (addr a) x x refl p (UnallocatedAddress a q) | ()

reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₀ f q) with (x ≡ⱽ y)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₀ f q) | yes r = function₀ f (λ s → q (trans s (substitutivityᴮ-unless-yes C v x y r p (⊕-over r))))
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₀ f q) | no r = function₀ f (λ s → q (trans s (substitutivityᴮ-unless-no C v x y r p (⊕-swap r))))
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₁ f W) with (x ≡ⱽ y)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₁ f W) | yes r = function₁ f (reflect-substitutionᴮ-unless-yes C v x y r p (⊕-over r) W)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₁ f W) | no r = function₁ f (reflect-substitutionᴮ-unless-no C v x y r p (⊕-swap r) W)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₂ f W) with (x ≡ⱽ f)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₂ f W)| yes r = function₂ f (reflect-substitutionᴮ-unless-yes B v x f r p (⊕-over r) W)
reflect-substitutionᴮ (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₂ f W)| no r = function₂ f (reflect-substitutionᴮ-unless-no B v x f r p (⊕-swap r) W)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p (local₀ q) = local₀ (λ r → q (trans r (substitutivityᴱ M v x p)))
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p (local₁ W) = local₁ (reflect-substitutionᴱ M v x p W)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p (local₂ W) with (x ≡ⱽ y)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p (local₂ W) | yes r = local₂ (reflect-substitutionᴮ-unless-yes B v x y r p (⊕-over r) W)
reflect-substitutionᴮ (local var y ∈ T ← M ∙ B) v x p (local₂ W) | no r = local₂ (reflect-substitutionᴮ-unless-no B v x y r p (⊕-swap r) W)
reflect-substitutionᴮ (return M ∙ B) v x p (return W) = return (reflect-substitutionᴱ M v x p W)

reflect-substitutionᴮ-unless-yes B v x y r p refl W = W
reflect-substitutionᴮ-unless-no B v x y r p refl W = reflect-substitutionᴮ B v x p W

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

reflectᴱ (app₁ s) (app₀ p) with preservationᴱ s | heap-weakeningᴱ (rednᴱ⊑ s)
reflectᴱ (app₁ s) (app₀ p) | ok q | ok q′ = expr (app₀ (λ r → p (trans (trans (cong src (sym q)) r) q′)))
reflectᴱ (app₁ s) (app₀ p) | warning (expr W) | _ = expr (app₁ W)
reflectᴱ (app₁ s) (app₀ p) | warning (heap W) | _ = heap W
reflectᴱ (app₁ s) (app₀ p) | _ | warning W  = expr (app₂ W)
reflectᴱ (app₁ s) (app₁ W′) with reflectᴱ s W′
reflectᴱ (app₁ s) (app₁ W′) | heap W = heap W
reflectᴱ (app₁ s) (app₁ W′) | expr W = expr (app₁ W)
reflectᴱ (app₁ s) (app₂ W′) = expr (app₂ (reflect-weakeningᴱ (rednᴱ⊑ s) W′))
reflectᴱ (app₂ p s) (app₀ p′) with heap-weakeningᴱ (rednᴱ⊑ s) | preservationᴱ s
reflectᴱ (app₂ p s) (app₀ p′) | ok q | ok q′ = expr (app₀ (λ r → p′ (trans (trans (cong src (sym q)) r) q′)))
reflectᴱ (app₂ p s) (app₀ p′) | warning W | _ = expr (app₁ W)
reflectᴱ (app₂ p s) (app₀ p′) | _ | warning (expr W)  = expr (app₂ W)
reflectᴱ (app₂ p s) (app₀ p′) | _ | warning (heap W)  = heap W
reflectᴱ (app₂ p s) (app₁ W′) = expr (app₁ (reflect-weakeningᴱ (rednᴱ⊑ s) W′))
reflectᴱ (app₂ p s) (app₂ W′) with reflectᴱ s W′
reflectᴱ (app₂ p s) (app₂ W′) | heap W = heap W
reflectᴱ (app₂ p s) (app₂ W′) | expr W = expr (app₂ W)
reflectᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} {B = B} {V = V} p refl) (block (var f ∈ U) W′) with remember (typeOfⱽ H V)
reflectᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} {B = B} {V = V} p refl) (block (var f ∈ U) W′) | (just S , q) with S ≡ᵀ T
reflectᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} {B = B} {V = V} p refl) (block (var f ∈ U) W′) | (just T , q) | yes refl = heap (addr a p (function₁ f (reflect-substitutionᴮ B V x (sym q) W′)))
reflectᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} {B = B} {V = V} p refl) (block (var f ∈ U) W′) | (just S , q) | no r = expr (app₀ (λ s → r (trans (cong orBot (sym q)) (trans (sym (typeOfᴱⱽ V)) (trans (sym s) (cong src (cong orBot (cong typeOfᴹᴼ p))))))))
reflectᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} {B = B} {V = V} p refl) (block (var f ∈ U) W′) | (nothing , q) with typeOf-val-not-bot V
reflectᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} {B = B} {V = V} p refl) (block (var f ∈ U) W′) | (nothing , q) | ok r = CONTRADICTION (r (trans (cong orBot (sym q)) (sym (typeOfᴱⱽ V))))
reflectᴱ {H = H} (beta {a = a} {F = f ⟨ var x ∈ T ⟩∈ U} {B = B} {V = V} p refl) (block (var f ∈ U) W′) | (nothing , q) | warning W = expr (app₂ W)
reflectᴱ (block s) (block b W′) with reflectᴮ s W′
reflectᴱ (block s) (block b W′) | heap W = heap W
reflectᴱ (block s) (block b W′) | block W = expr (block b W)
reflectᴱ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) (UnallocatedAddress a ())
reflectᴱ (return q) W = expr (block _ (return W))

reflectᴮ (local s) (local₀ p) with preservationᴱ s
reflectᴮ (local s) (local₀ p) | ok q = block (local₀ (λ r → p (trans r q)))
reflectᴮ (local s) (local₀ p) | warning (expr W) = block (local₁ W)
reflectᴮ (local s) (local₀ p) | warning (heap W) = heap W
reflectᴮ (local s) (local₁ W′) with reflectᴱ s W′
reflectᴮ (local s) (local₁ W′) | heap W = heap W
reflectᴮ (local s) (local₁ W′) | expr W = block (local₁ W)
reflectᴮ (local s) (local₂ W′) = block (local₂ (reflect-weakeningᴮ (rednᴱ⊑ s) W′))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = v}) W with just T ≡ᴹᵀ typeOfⱽ H v
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = v}) W | yes p = block (local₂ (reflect-substitutionᴮ _ v x p W))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = nil}) W | no p = block (local₀ λ r → p (cong just r))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = addr a}) W | no p with remember(H [ a ]ᴴ)
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = addr a}) W | no p | (nothing , q) = block (local₁ (UnallocatedAddress a q))
reflectᴮ (subst {H = H} {x = var x ∈ T} {v = addr a}) W | no p | (just O , q) = block (local₀ (λ r → p (trans (cong just (trans r (cong orBot (cong typeOfᴹᴼ q)))) (cong typeOfᴹᴼ (sym q)))))
reflectᴮ (function {F = f ⟨ var x ∈ S ⟩∈ T} defn) W = block (function₂ f (reflect-weakeningᴮ (snoc defn) (reflect-substitutionᴮ _ _ f refl W)))
reflectᴮ (return s) (return W′) with reflectᴱ s W′
reflectᴮ (return s) (return W′) | heap W = heap W
reflectᴮ (return s) (return W′) | expr W = block (return W)

reflectᴴᴱ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴴᴱ H′ (typeCheckᴴᴱ H′ ∅ M′) → Warningᴴᴱ H (typeCheckᴴᴱ H ∅ M)
reflectᴴᴮ : ∀ {H H′ B B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)

reflectᴴᴱ s (expr W′) = reflectᴱ s W′
reflectᴴᴱ (function {a = a} p) (heap (addr b refl W′)) with b ≡ᴬ a
reflectᴴᴱ (function defn) (heap (addr a refl (function₀ f p))) | yes refl with heap-weakeningᴮ (snoc defn)
reflectᴴᴱ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | ok r = expr (function₀ f λ q → p (trans q r))
reflectᴴᴱ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | warning W = expr (function₁ f W)
reflectᴴᴱ (function defn) (heap (addr a refl (function₁ f W′))) | yes refl = expr (function₁ f (reflect-weakeningᴮ (snoc defn) W′))
reflectᴴᴱ (function p) (heap (addr b refl W′)) | no r = heap (addr b (lookup-not-allocated p r) (reflect-weakeningᴼ (snoc p) W′))
reflectᴴᴱ (app₁ s) (heap W′) with reflectᴴᴱ s (heap W′)
reflectᴴᴱ (app₁ s) (heap W′) | heap W = heap W
reflectᴴᴱ (app₁ s) (heap W′) | expr W = expr (app₁ W)
reflectᴴᴱ (app₂ p s) (heap W′) with reflectᴴᴱ s (heap W′)
reflectᴴᴱ (app₂ p s) (heap W′) | heap W = heap W
reflectᴴᴱ (app₂ p s) (heap W′) | expr W = expr (app₂ W)
reflectᴴᴱ (beta p q) (heap W′) = heap W′
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
reflectᴴᴮ (function defn) (heap (addr a refl (function₀ f p))) | yes refl with heap-weakeningᴮ (snoc defn)
reflectᴴᴮ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | ok r = block (function₀ f (λ q → p (trans q r)))
reflectᴴᴮ (function defn) (heap (addr a refl (function₀ f p))) | yes refl | warning W = block (function₁ f W)
reflectᴴᴮ (function defn) (heap (addr a refl (function₁ f W′))) | yes refl = block (function₁ f (reflect-weakeningᴮ (snoc defn) W′))
reflectᴴᴮ (function p) (heap (addr b refl W′)) | no r = heap (addr b (lookup-not-allocated p r) (reflect-weakeningᴼ (snoc p) W′))
reflectᴴᴮ (return s) (heap W′) with reflectᴴᴱ s (heap W′)
reflectᴴᴮ (return s) (heap W′) | heap W = heap W
reflectᴴᴮ (return s) (heap W′) | expr W = block (return W)

reflect* : ∀ {H H′ B B′} → (H ⊢ B ⟶* B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)
reflect* refl W = W
reflect* (step s t) W = reflectᴴᴮ s (reflect* t W)

runtimeWarningᴱ : ∀ {H M} → RuntimeErrorᴱ H M → Warningᴱ H (typeCheckᴱ H ∅ M)
runtimeWarningᴮ : ∀ {H B} → RuntimeErrorᴮ H B → Warningᴮ H (typeCheckᴮ H ∅ B)

runtimeWarningᴱ (NilIsNotAFunction {V = V}) with typeOf-val-not-bot V
runtimeWarningᴱ (NilIsNotAFunction) | ok p = app₀ p
runtimeWarningᴱ (NilIsNotAFunction) | warning W = app₂ W
runtimeWarningᴱ (UnboundVariable x) = UnboundVariable x refl
runtimeWarningᴱ (SEGV a p) = UnallocatedAddress a p
runtimeWarningᴱ (app₁ err) = app₁ (runtimeWarningᴱ err)
runtimeWarningᴱ (app₂ err) = app₂ (runtimeWarningᴱ err)
runtimeWarningᴱ (block b err) = block b (runtimeWarningᴮ err)

runtimeWarningᴮ (local (var x ∈ T) err) = local₁ (runtimeWarningᴱ err)
runtimeWarningᴮ (return err) = return (runtimeWarningᴱ err)

wellTypedProgramsDontGoWrong : ∀ {H′ B B′} → (∅ᴴ ⊢ B ⟶* B′ ⊣ H′) → (RuntimeErrorᴮ H′ B′) → Warningᴮ ∅ᴴ (typeCheckᴮ ∅ᴴ ∅ B)
wellTypedProgramsDontGoWrong t err with reflect* t (block (runtimeWarningᴮ err))
wellTypedProgramsDontGoWrong t err | heap (addr a refl ())
wellTypedProgramsDontGoWrong t err | block W = W
