{-# OPTIONS --rewriting #-}
{-# OPTIONS --allow-unsolved-metas #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; Object; function_is_end; defn; alloc; ok; next; lookup-not-allocated) renaming (_≡_⊕_↦_ to _≡ᴴ_⊕_↦_; _[_] to _[_]ᴴ; ∅ to ∅ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; Warningᴼ; Warningᴴᴱ; Warningᴴᴮ; UnallocatedAddress; UnboundVariable; app₀; app₁; app₂; block₀; block₁; return; local₀; local₁; local₂; function₀; function₁; function₂; heap; expr; block; addr)
open import Luau.Substitution using (_[_/_]ᴮ; _[_/_]ᴱ; _[_/_]ᴮunless_; var_[_/_]ᴱwhenever_)
open import Luau.Syntax using (Expr; yes; var; var_∈_; _⟨_⟩∈_; _$_; addr; number; binexp; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg; name)
open import Luau.Type using (Type; strict; nil; _⇒_; bot; tgt; _≡ᵀ_; _≡ᴹᵀ_)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∈_; _⊢ᴱ_∈_; _⊢ᴴᴮ_▷_∈_; _⊢ᴴᴱ_▷_∈_; nil; var; addr; app; function; block; done; return; local; orBot)
open import Luau.Value using (val; nil; addr; number)
open import Luau.Var using (_≡ⱽ_)
open import Luau.Addr using (_≡ᴬ_)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_; ⊕-lookup-miss; ⊕-swap; ⊕-over) renaming (_[_] to _[_]ⱽ)
open import Luau.VarCtxt using (VarCtxt; ∅)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (_≢_; sym; cong; trans; subst₁)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.TypeCheck(strict) using (typeOfᴼ; typeOfᴹᴼ; typeOfⱽ; typeOfᴱ; typeOfᴮ; typeOfᴱⱽ; typeCheckᴱ; typeCheckᴮ; typeCheckᴼ; typeCheckᴴᴱ; typeCheckᴴᴮ)
open import Luau.OpSem using (_⊢_⟶*_⊣_; _⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app₁; app₂; function; beta; return; block; done; local; subst; binOp₁; binOp₂; binOpEval; refl; step)
open import Luau.RuntimeError using (RuntimeErrorᴱ; RuntimeErrorᴮ; FunctionMismatch; BinopMismatch₁; BinopMismatch₂; UnboundVariable; SEGV; app₁; app₂; bin₁; bin₂; block; local; return)

src = Luau.Type.src strict

data _⊑_ (H : Heap yes) : Heap yes → Set where
  refl : (H ⊑ H)
  snoc : ∀ {H′ a V} → (H′ ≡ᴴ H ⊕ a ↦ V) → (H ⊑ H′)

rednᴱ⊑ : ∀ {H H′ M M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → (H ⊑ H′)
rednᴮ⊑ : ∀ {H H′ B B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → (H ⊑ H′)

rednᴱ⊑ (function a p) = snoc p
rednᴱ⊑ (app₁ s) = rednᴱ⊑ s
rednᴱ⊑ (app₂ p s) = rednᴱ⊑ s
rednᴱ⊑ (beta O v p q) = refl
rednᴱ⊑ (block s) = rednᴮ⊑ s
rednᴱ⊑ (return v) = refl
rednᴱ⊑ done = refl
rednᴱ⊑ (binOpEval) = refl
rednᴱ⊑ (binOp₁ s) = rednᴱ⊑ s
rednᴱ⊑ (binOp₂ s) = rednᴱ⊑ s

rednᴮ⊑ (local s) = rednᴱ⊑ s
rednᴮ⊑ (subst v) = refl
rednᴮ⊑ (function a p) = snoc p
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

heap-weakeningᴱ : ∀ H M {H′ Γ} → (H ⊑ H′) → OrWarningᴱ H (typeCheckᴱ H Γ M) (typeOfᴱ H Γ M ≡ typeOfᴱ H′ Γ M)
heap-weakeningᴮ : ∀ H B {H′ Γ} → (H ⊑ H′) → OrWarningᴮ H (typeCheckᴮ H Γ B) (typeOfᴮ H Γ B ≡ typeOfᴮ H′ Γ B)

heap-weakeningᴱ H (nil) h = ok refl
heap-weakeningᴱ H (var x) h = ok refl
heap-weakeningᴱ H (addr a) refl = ok refl
heap-weakeningᴱ H (addr a) (snoc {a = b} defn) with a ≡ᴬ b
heap-weakeningᴱ H (addr a) (snoc {a = a} defn) | yes refl = warning (UnallocatedAddress a refl)
heap-weakeningᴱ H (addr a) (snoc {a = b} p) | no q = ok (cong orBot (cong typeOfᴹᴼ (lookup-not-allocated p q)))
heap-weakeningᴱ H (number n) h = ok refl
heap-weakeningᴱ H (binexp M op N) h = ok refl
heap-weakeningᴱ H (M $ N) h with heap-weakeningᴱ H M h
heap-weakeningᴱ H (M $ N) h | ok p = ok (cong tgt p)
heap-weakeningᴱ H (M $ N) h | warning W = warning (app₁ W)
heap-weakeningᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) h = ok refl
heap-weakeningᴱ H (block var b ∈ T is B end) h = ok refl
heap-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h with heap-weakeningᴮ H B h
heap-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h | ok p = ok p
heap-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h | warning W = warning (function₂ f W)
heap-weakeningᴮ H (local var x ∈ T ← M ∙ B) h with heap-weakeningᴮ H B h
heap-weakeningᴮ H (local var x ∈ T ← M ∙ B) h | ok p = ok p
heap-weakeningᴮ H (local var x ∈ T ← M ∙ B) h | warning W = warning (local₂ W)
heap-weakeningᴮ H (return M ∙ B) h with heap-weakeningᴱ H M h
heap-weakeningᴮ H (return M ∙ B) h | ok p = ok p
heap-weakeningᴮ H (return M ∙ B) h | warning W = warning (return W)
heap-weakeningᴮ H (done) h = ok refl

bot-not-obj : ∀ O → bot ≢ typeOfᴼ O
bot-not-obj (function f ⟨ var x ∈ T ⟩∈ U is B end) ()

typeOf-val-not-bot : ∀ {H Γ} v → OrWarningᴱ H (typeCheckᴱ H Γ (val v)) (bot ≢ typeOfᴱ H Γ (val v))
typeOf-val-not-bot nil = ok (λ ())
typeOf-val-not-bot (number n) = ok (λ ())
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
substitutivityᴱ (number n) v x p = refl
substitutivityᴱ (binexp M op N) v x p = refl
substitutivityᴱ (M $ N) v x p = cong tgt (substitutivityᴱ M v x p)
substitutivityᴱ (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p = refl
substitutivityᴱ (block var b ∈ T is B end) v x p = refl
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

preservationᴱ : ∀ H M {H′ M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → OrWarningᴴᴱ H (typeCheckᴴᴱ H ∅ M) (typeOfᴱ H ∅ M ≡ typeOfᴱ H′ ∅ M′)
preservationᴮ : ∀ H B {H′ B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → OrWarningᴴᴮ H (typeCheckᴴᴮ H ∅ B) (typeOfᴮ H ∅ B ≡ typeOfᴮ H′ ∅ B′)

preservationᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) = ok refl
preservationᴱ H (M $ N) (app₁ s) with preservationᴱ H M s
preservationᴱ H (M $ N) (app₁ s) | ok p = ok (cong tgt p)
preservationᴱ H (M $ N) (app₁ s) | warning (expr W) = warning (expr (app₁ W))
preservationᴱ H (M $ N) (app₁ s) | warning (heap W) = warning (heap W)
preservationᴱ H (M $ N) (app₂ p s) with heap-weakeningᴱ H M (rednᴱ⊑ s)
preservationᴱ H (M $ N) (app₂ p s) | ok q = ok (cong tgt q)
preservationᴱ H (M $ N) (app₂ p s) | warning W  = warning (expr (app₁ W))
preservationᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) with remember (typeOfⱽ H v)
preservationᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) with S ≡ᵀ U | T ≡ᵀ typeOfᴮ H (x ↦ S) B
preservationᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) | yes refl | yes refl = ok (trans (cong tgt (cong orBot (cong typeOfᴹᴼ p))) {!!}) --  (substitutivityᴮ H B v x (sym q)))
preservationᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) | yes refl | no r = warning (heap (addr a p (function₀ f r)))
preservationᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) | no r | _ = warning (expr (app₀ (λ s → r (trans (trans (sym (cong src (cong orBot (cong typeOfᴹᴼ p)))) (trans s (typeOfᴱⱽ v))) (cong orBot q)))))
preservationᴱ H (addr a $ N)  (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (nothing , q) with typeOf-val-not-bot v
preservationᴱ H (addr a $ N)  (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (nothing , q) | ok r = CONTRADICTION (r (sym (trans (typeOfᴱⱽ v) (cong orBot q))))
preservationᴱ H (addr a $ N)  (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (nothing , q) | warning W = warning (expr (app₂ W))
preservationᴱ H (block var b ∈ T is B end) (block s) = ok refl
preservationᴱ H (block var b ∈ T is return M ∙ B end) (return v) = {!!} -- ok refl
preservationᴱ H (block var b ∈ T is done end) (done) = {!!} -- ok refl
preservationᴱ H (binexp M op N) s = {!!}

preservationᴮ H (local var x ∈ T ← M ∙ B) (local s) with heap-weakeningᴮ H {!!} (rednᴱ⊑ s)
preservationᴮ H (local var x ∈ T ← M ∙ B) (local s) | ok p = ok p
preservationᴮ H (local var x ∈ T ← M ∙ B) (local s) | warning W = warning (block (local₂ W))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) with remember (typeOfⱽ H v)
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (just U , p) with T ≡ᵀ U
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (just T , p) | yes refl = ok (substitutivityᴮ B v x (sym p))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (just U , p) | no q = warning (block (local₀ (λ r → q (trans r (trans (typeOfᴱⱽ v) (cong orBot p))))))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (nothing , p) with typeOf-val-not-bot v
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (nothing , p) | ok q = CONTRADICTION (q (sym (trans (typeOfᴱⱽ v) (cong orBot p))))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (nothing , p) | warning W = warning (block (local₁ W))
preservationᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) with heap-weakeningᴮ H {!!} (snoc defn)
preservationᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) | ok r = ok (trans r (substitutivityᴮ {T = T ⇒ U} B (addr a) f refl))
preservationᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) | warning W = warning (block (function₂ f W))
preservationᴮ H (return M ∙ B) (return s) with preservationᴱ H M s
preservationᴮ H (return M ∙ B) (return s) | ok p = ok p
preservationᴮ H (return M ∙ B) (return s) | warning (expr W) = warning (block (return W))
preservationᴮ H (return M ∙ B) (return s) | warning (heap W) = warning (heap W)

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
reflect-substitutionᴱ (block var b ∈ T is B end) v x p (block₀ b W) = {!!}
reflect-substitutionᴱ (block var b ∈ T is B end) v x p (block₁ b W) = block₁ b (reflect-substitutionᴮ B v x p W)
reflect-substitutionᴱ (binexp M op N) x v p W = {!!}

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

reflect-weakeningᴱ : ∀ H M {H′ Γ} → (H ⊑ H′) → Warningᴱ H′ (typeCheckᴱ H′ Γ M) → Warningᴱ H (typeCheckᴱ H Γ M)
reflect-weakeningᴮ : ∀ H B {H′ Γ} → (H ⊑ H′) → Warningᴮ H′ (typeCheckᴮ H′ Γ B) → Warningᴮ H (typeCheckᴮ H Γ B)

reflect-weakeningᴱ H (var x) h (UnboundVariable x p) = (UnboundVariable x p)
reflect-weakeningᴱ H (addr a) h (UnallocatedAddress a p) = UnallocatedAddress a (lookup-⊑-nothing a h p)
reflect-weakeningᴱ H (M $ N) h (app₀ p) with heap-weakeningᴱ H {!!} h | heap-weakeningᴱ H {!!} h
reflect-weakeningᴱ H (M $ N) h (app₀ p) | ok q₁ | ok q₂ = app₀ (λ r → p (trans (cong src (sym q₁)) (trans r q₂)))
reflect-weakeningᴱ H (M $ N) h (app₀ p) | warning W | _ = app₁ W
reflect-weakeningᴱ H (M $ N) h (app₀ p) | _ | warning W = app₂ W
reflect-weakeningᴱ H (M $ N) h (app₁ W) = app₁ (reflect-weakeningᴱ H M h W)
reflect-weakeningᴱ H (M $ N) h (app₂ W) = app₂ (reflect-weakeningᴱ H N h W)
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (function₀ f p) with heap-weakeningᴮ H {!!} h
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (function₀ f p) | ok q = function₀ f (λ r → p (trans r q))
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (function₀ f p) | warning W = function₁ f W
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (function₁ f W) = function₁ f (reflect-weakeningᴮ H B h W)
reflect-weakeningᴱ H (block var b ∈ T is B end) h (block₀ b W) = {!!} -- block₁ b (reflect-weakeningᴮ H B h W)
reflect-weakeningᴱ H (block var b ∈ T is B end) h (block₁ b W) = block₁ b (reflect-weakeningᴮ H B h W)

reflect-weakeningᴮ H (return M ∙ B) h (return W) = return (reflect-weakeningᴱ H M h W)
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (local₀ p) with heap-weakeningᴱ H {!!} h
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (local₀ p) | ok q = local₀ (λ r → p (trans r q))
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (local₀ p) | warning W = local₁ W
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (local₁ W) = local₁ (reflect-weakeningᴱ H M h W)
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (local₂ W) = local₂ (reflect-weakeningᴮ H B h W)
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₀ f p) with heap-weakeningᴮ H {!!} h
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₀ f p) | ok q = function₀ f (λ r → p (trans r q))
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₀ f p) | warning W = function₁ f W
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₁ f W) = function₁ f (reflect-weakeningᴮ H C h W)
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₂ f W) = function₂ f (reflect-weakeningᴮ H B h W)

reflect-weakeningᴼ : ∀ H O {H′} → (H ⊑ H′) → Warningᴼ H′ (typeCheckᴼ H′ O) → Warningᴼ H (typeCheckᴼ H O)
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (function₀ f p) with heap-weakeningᴮ H {!!} h
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (function₀ f p) | ok q = function₀ f (λ r → p (trans r q))
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (function₀ f p) | warning W = function₁ f W
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (function₁ f W′) = function₁ f (reflect-weakeningᴮ H B h W′)

reflectᴱ : ∀ H M {H′ M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴱ H′ (typeCheckᴱ H′ ∅ M′) → Warningᴴᴱ H (typeCheckᴴᴱ H ∅ M)
reflectᴮ : ∀ H B {H′ B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴮ H′ (typeCheckᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)

reflectᴱ H (M $ N) (app₁ s) (app₀ p) with preservationᴱ H M s | heap-weakeningᴱ H {!!} (rednᴱ⊑ s)
reflectᴱ H (M $ N) (app₁ s) (app₀ p) | ok q | ok q′ = expr (app₀ (λ r → p (trans (trans (cong src (sym q)) r) q′)))
reflectᴱ H (M $ N) (app₁ s) (app₀ p) | warning (expr W) | _ = expr (app₁ W)
reflectᴱ H (M $ N) (app₁ s) (app₀ p) | warning (heap W) | _ = heap W
reflectᴱ H (M $ N) (app₁ s) (app₀ p) | _ | warning W  = expr (app₂ W)
reflectᴱ H (M $ N) (app₁ s) (app₁ W′) with reflectᴱ H M s W′
reflectᴱ H (M $ N) (app₁ s) (app₁ W′) | heap W = heap W
reflectᴱ H (M $ N) (app₁ s) (app₁ W′) | expr W = expr (app₁ W)
reflectᴱ H (M $ N) (app₁ s) (app₂ W′) = expr (app₂ (reflect-weakeningᴱ H N (rednᴱ⊑ s) W′))
reflectᴱ H (M $ N) (app₂ p s) (app₀ p′) with heap-weakeningᴱ H {!!} (rednᴱ⊑ s) | preservationᴱ H N s
reflectᴱ H (M $ N) (app₂ p s) (app₀ p′) | ok q | ok q′ = expr (app₀ (λ r → p′ (trans (trans (cong src (sym q)) r) q′)))
reflectᴱ H (M $ N) (app₂ p s) (app₀ p′) | warning W | _ = expr (app₁ W)
reflectᴱ H (M $ N) (app₂ p s) (app₀ p′) | _ | warning (expr W)  = expr (app₂ W)
reflectᴱ H (M $ N) (app₂ p s) (app₀ p′) | _ | warning (heap W)  = heap W
reflectᴱ H (M $ N) (app₂ p s) (app₁ W′) = expr (app₁ (reflect-weakeningᴱ H M (rednᴱ⊑ s) W′))
reflectᴱ H (M $ N) (app₂ p s) (app₂ W′) with reflectᴱ H N s W′
reflectᴱ H (M $ N) (app₂ p s) (app₂ W′) | heap W = heap W
reflectᴱ H (M $ N) (app₂ p s) (app₂ W′) | expr W = expr (app₂ W)
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₀ f W′) = {!!}
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ f W′) with remember (typeOfⱽ H v)
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ f W′) | (just S , q) with S ≡ᵀ T
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ f W′) | (just T , q) | yes refl = heap (addr a p (function₁ f (reflect-substitutionᴮ B v x (sym q) W′)))
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ f W′) | (just S , q) | no r = expr (app₀ (λ s → r (trans (cong orBot (sym q)) (trans (sym (typeOfᴱⱽ v)) (trans (sym s) (cong src (cong orBot (cong typeOfᴹᴼ p))))))))
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ f W′) | (nothing , q) with typeOf-val-not-bot v
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ f W′) | (nothing , q) | ok r = CONTRADICTION (r (trans (cong orBot (sym q)) (sym (typeOfᴱⱽ v))))
reflectᴱ H (addr a $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ f W′) | (nothing , q) | warning W = expr (app₂ W)
reflectᴱ H (block var b ∈ T is B end) (block s) (block₀ b W′) = {!!}
reflectᴱ H (block var b ∈ T is B end) (block s) (block₁ b W′) with reflectᴮ H B s W′
reflectᴱ H (block var b ∈ T is B end) (block s) (block₁ b W′) | heap W = heap W
reflectᴱ H (block var b ∈ T is B end) (block s) (block₁ b W′) | block W = expr (block₁ b W)
reflectᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (UnallocatedAddress a ())
reflectᴱ H (block var b ∈ T is return N ∙ B end) W = {!!} -- expr (block₁ _ (return W))

reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₀ p) with preservationᴱ H M s
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₀ p) | ok q = block (local₀ (λ r → p (trans r q)))
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₀ p) | warning (expr W) = block (local₁ W)
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₀ p) | warning (heap W) = heap W
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₁ W′) with reflectᴱ H M s W′
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₁ W′) | heap W = heap W
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₁ W′) | expr W = block (local₁ W)
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₂ W′) = block (local₂ (reflect-weakeningᴮ H B (rednᴱ⊑ s) W′))
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W with just T ≡ᴹᵀ typeOfⱽ H v
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W | yes p = block (local₂ (reflect-substitutionᴮ _ v x p W))
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W | no p = {!!} -- block (local₀ λ r → p (cong just r))
reflectᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) W = {!!} -- block (function₂ f (reflect-weakeningᴮ H (snoc defn) (reflect-substitutionᴮ _ _ f refl W)))
reflectᴮ H (return M ∙ B) (return s) (return W′) with reflectᴱ H M s W′
reflectᴮ H (return M ∙ B) (return s) (return W′) | heap W = heap W
reflectᴮ H (return M ∙ B) (return s) (return W′) | expr W = block (return W)

reflectᴴᴱ : ∀ H M {H′ M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴴᴱ H′ (typeCheckᴴᴱ H′ ∅ M′) → Warningᴴᴱ H (typeCheckᴴᴱ H ∅ M)
reflectᴴᴮ : ∀ H B {H′ B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)

reflectᴴᴱ H M s (expr W′) = reflectᴱ H M s W′
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a p) (heap (addr b refl W′)) with b ≡ᴬ a
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (function₀ f p))) | yes refl with heap-weakeningᴮ H {!!} (snoc defn)
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (function₀ f p))) | yes refl | ok r = expr (function₀ f λ q → p (trans q r))
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (function₀ f p))) | yes refl | warning W = expr (function₁ f W)
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (function₁ f W′))) | yes refl = expr (function₁ f (reflect-weakeningᴮ H B (snoc defn) W′))
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a p) (heap (addr b refl W′)) | no r = heap (addr b (lookup-not-allocated p r) (reflect-weakeningᴼ H _ (snoc p) W′))
reflectᴴᴱ H (M $ N) (app₁ s) (heap W′) with reflectᴴᴱ H M s (heap W′)
reflectᴴᴱ H (M $ N) (app₁ s) (heap W′) | heap W = heap W
reflectᴴᴱ H (M $ N) (app₁ s) (heap W′) | expr W = expr (app₁ W)
reflectᴴᴱ H (M $ N) (app₂ p s) (heap W′) with reflectᴴᴱ H N s (heap W′)
reflectᴴᴱ H (M $ N) (app₂ p s) (heap W′) | heap W = heap W
reflectᴴᴱ H (M $ N) (app₂ p s) (heap W′) | expr W = expr (app₂ W)
reflectᴴᴱ H (M $ N) (beta O v p q) (heap W′) = heap W′
reflectᴴᴱ H (block var b ∈ T is B end) (block s) (heap W′) with reflectᴴᴮ H B s (heap W′)
reflectᴴᴱ H (block var b ∈ T is B end) (block s) (heap W′) | heap W = heap W
reflectᴴᴱ H (block var b ∈ T is B end) (block s) (heap W′) | block W = expr (block₁ b W)
reflectᴴᴱ H (block var b ∈ T is return N ∙ B end) (return v) (heap W′) = heap W′
reflectᴴᴱ H (block var b ∈ T is done end) done (heap W′) = heap W′
reflectᴴᴱ H (binexp M op N) s W′ = {!!}

reflectᴴᴮ H B s (block W′) = reflectᴮ H B s W′
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (local s) (heap W′) with reflectᴴᴱ H M s (heap W′)
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (local s) (heap W′) | heap W = heap W
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (local s) (heap W′) | expr W = block (local₁ W)
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (subst v) (heap W′) = heap W′
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a p) (heap (addr b refl W′)) with b ≡ᴬ a
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (function₀ f p))) | yes refl with heap-weakeningᴮ H {!!} (snoc defn)
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (function₀ f p))) | yes refl | ok r = block (function₀ f (λ q → p (trans q r)))
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (function₀ f p))) | yes refl | warning W = block (function₁ f W)
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (function₁ f W′))) | yes refl = block (function₁ f (reflect-weakeningᴮ H C (snoc defn) W′))
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a p) (heap (addr b refl W′)) | no r = heap (addr b (lookup-not-allocated p r) (reflect-weakeningᴼ H _ (snoc p) W′))
reflectᴴᴮ H (return M ∙ B) (return s) (heap W′) with reflectᴴᴱ H M s (heap W′)
reflectᴴᴮ H (return M ∙ B) (return s) (heap W′) | heap W = heap W
reflectᴴᴮ H (return M ∙ B) (return s) (heap W′) | expr W = block (return W)

reflect* : ∀ H B {H′ B′} → (H ⊢ B ⟶* B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)
reflect* H B refl W = W
reflect* H B (step s t) W = reflectᴴᴮ H B s (reflect* _ _ t W)

runtimeWarningᴱ : ∀ H M → RuntimeErrorᴱ H M → Warningᴱ H (typeCheckᴱ H ∅ M)
runtimeWarningᴮ : ∀ H B → RuntimeErrorᴮ H B → Warningᴮ H (typeCheckᴮ H ∅ B)

runtimeWarningᴱ H (var x) UnboundVariable = UnboundVariable x refl
runtimeWarningᴱ H (addr a) (SEGV p) = UnallocatedAddress a p
runtimeWarningᴱ H (M $ N) (FunctionMismatch v w r) = {!!} -- app₁ (runtimeWarningᴱ H M err)
runtimeWarningᴱ H (M $ N) (app₁ err) = app₁ (runtimeWarningᴱ H M err)
runtimeWarningᴱ H (M $ N) (app₂ err) = app₂ (runtimeWarningᴱ H N err)
runtimeWarningᴱ H (block var b ∈ T is B end) (block err) = block₁ b (runtimeWarningᴮ H B err)
runtimeWarningᴱ H (binexp M op N) (BinopMismatch₁ v w p) = {!!}
runtimeWarningᴱ H (binexp M op N) (BinopMismatch₂ v w p) = {!!}
runtimeWarningᴱ H (binexp M op N) (bin₁ err) = {!!}
runtimeWarningᴱ H (binexp M op N) (bin₂ err) = {!!}

runtimeWarningᴮ H (local var x ∈ T ← M ∙ B) (local err) = local₁ (runtimeWarningᴱ H M err)
runtimeWarningᴮ H (return M ∙ B) (return err) = return (runtimeWarningᴱ H M err)

wellTypedProgramsDontGoWrong : ∀ H′ B B′ → (∅ᴴ ⊢ B ⟶* B′ ⊣ H′) → (RuntimeErrorᴮ H′ B′) → Warningᴮ ∅ᴴ (typeCheckᴮ ∅ᴴ ∅ B)
wellTypedProgramsDontGoWrong H′ B B′ t err with reflect* ∅ᴴ B t (block (runtimeWarningᴮ H′ B′ err))
wellTypedProgramsDontGoWrong H′ B B′ t err | heap (addr a refl ())
wellTypedProgramsDontGoWrong H′ B B′ t err | block W = W
