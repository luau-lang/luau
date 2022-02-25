{-# OPTIONS --rewriting #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; Object; function_is_end; defn; alloc; ok; next; lookup-not-allocated) renaming (_≡_⊕_↦_ to _≡ᴴ_⊕_↦_; _[_] to _[_]ᴴ; ∅ to ∅ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; Warningᴼ; Warningᴴᴱ; Warningᴴᴮ; UnallocatedAddress; UnboundVariable; FunctionCallMismatch; app₁; app₂; BinOpWarning; BinOpMismatch₁; BinOpMismatch₂; bin₁; bin₂; BlockMismatch; block₁; return; LocalVarMismatch; local₁; local₂; FunctionDefnMismatch; function₁; function₂; heap; expr; block; addr; +; -; *; /; <; >; <=; >=)
open import Luau.Substitution using (_[_/_]ᴮ; _[_/_]ᴱ; _[_/_]ᴮunless_; var_[_/_]ᴱwhenever_)
open import Luau.Syntax using (Expr; yes; var; val; var_∈_; _⟨_⟩∈_; _$_; addr; number; bool; binexp; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg; name; ==; ~=)
open import Luau.Type using (Type; strict; nil; _⇒_; none; tgt; _≡ᵀ_; _≡ᴹᵀ_)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∈_; _⊢ᴱ_∈_; _⊢ᴴᴮ_▷_∈_; _⊢ᴴᴱ_▷_∈_; nil; var; addr; app; function; block; done; return; local; orNone; tgtBinOp)
open import Luau.Var using (_≡ⱽ_)
open import Luau.Addr using (_≡ᴬ_)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_; ⊕-lookup-miss; ⊕-swap; ⊕-over) renaming (_[_] to _[_]ⱽ)
open import Luau.VarCtxt using (VarCtxt; ∅)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (_≢_; sym; cong; trans; subst₁)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.TypeCheck(strict) using (typeOfᴼ; typeOfᴹᴼ; typeOfⱽ; typeOfᴱ; typeOfᴮ; typeCheckᴱ; typeCheckᴮ; typeCheckᴼ; typeCheckᴴᴱ; typeCheckᴴᴮ; mustBeFunction; mustBeNumber)
open import Luau.OpSem using (_⟦_⟧_⟶_; _⊢_⟶*_⊣_; _⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app₁; app₂; function; beta; return; block; done; local; subst; binOp₀; binOp₁; binOp₂; refl; step; +; -; *; /; <; >; ==; ~=; <=; >=)
open import Luau.RuntimeError using (BinOpError; RuntimeErrorᴱ; RuntimeErrorᴮ; FunctionMismatch; BinOpMismatch₁; BinOpMismatch₂; UnboundVariable; SEGV; app₁; app₂; bin₁; bin₂; block; local; return; +; -; *; /; <; >; <=; >=)
open import Luau.RuntimeType using (valueType)

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
rednᴱ⊑ (binOp₀ p) = refl
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

heap-weakeningᴱ H (var x) h = ok refl
heap-weakeningᴱ H (val nil) h = ok refl
heap-weakeningᴱ H (val (addr a)) refl = ok refl
heap-weakeningᴱ H (val (addr a)) (snoc {a = b} defn) with a ≡ᴬ b
heap-weakeningᴱ H (val (addr a)) (snoc {a = a} defn) | yes refl = warning (UnallocatedAddress refl)
heap-weakeningᴱ H (val (addr a)) (snoc {a = b} p) | no q = ok (cong orNone (cong typeOfᴹᴼ (lookup-not-allocated p q)))
heap-weakeningᴱ H (val (number n)) h = ok refl
heap-weakeningᴱ H (val (bool b)) h = ok refl
heap-weakeningᴱ H (binexp M op N) h = ok refl
heap-weakeningᴱ H (M $ N) h with heap-weakeningᴱ H M h
heap-weakeningᴱ H (M $ N) h | ok p = ok (cong tgt p)
heap-weakeningᴱ H (M $ N) h | warning W = warning (app₁ W)
heap-weakeningᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) h = ok refl
heap-weakeningᴱ H (block var b ∈ T is B end) h = ok refl
heap-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h with heap-weakeningᴮ H B h
heap-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h | ok p = ok p
heap-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h | warning W = warning (function₂ W)
heap-weakeningᴮ H (local var x ∈ T ← M ∙ B) h with heap-weakeningᴮ H B h
heap-weakeningᴮ H (local var x ∈ T ← M ∙ B) h | ok p = ok p
heap-weakeningᴮ H (local var x ∈ T ← M ∙ B) h | warning W = warning (local₂ W)
heap-weakeningᴮ H (return M ∙ B) h with heap-weakeningᴱ H M h
heap-weakeningᴮ H (return M ∙ B) h | ok p = ok p
heap-weakeningᴮ H (return M ∙ B) h | warning W = warning (return W)
heap-weakeningᴮ H (done) h = ok refl

none-not-obj : ∀ O → none ≢ typeOfᴼ O
none-not-obj (function f ⟨ var x ∈ T ⟩∈ U is B end) ()

typeOf-val-not-none : ∀ {H Γ} v → OrWarningᴱ H (typeCheckᴱ H Γ (val v)) (none ≢ typeOfᴱ H Γ (val v))
typeOf-val-not-none nil = ok (λ ())
typeOf-val-not-none (number n) = ok (λ ())
typeOf-val-not-none (bool b) = ok (λ ())
typeOf-val-not-none {H = H} (addr a) with remember (H [ a ]ᴴ)
typeOf-val-not-none {H = H} (addr a) | (just O , p) = ok (λ q → none-not-obj O (trans q (cong orNone (cong typeOfᴹᴼ p))))
typeOf-val-not-none {H = H} (addr a) | (nothing , p) = warning (UnallocatedAddress p)

substitutivityᴱ : ∀ {Γ T} H M v x → (just T ≡ typeOfⱽ H v) → (typeOfᴱ H (Γ ⊕ x ↦ T) M ≡ typeOfᴱ H Γ (M [ v / x ]ᴱ))
substitutivityᴱ-whenever-yes : ∀ {Γ T} H v x y (p : x ≡ y) → (just T ≡ typeOfⱽ H v) → (typeOfᴱ H (Γ ⊕ x ↦ T) (var y) ≡ typeOfᴱ H Γ (var y [ v / x ]ᴱwhenever (yes p)))
substitutivityᴱ-whenever-no : ∀ {Γ T} H v x y (p : x ≢ y) → (just T ≡ typeOfⱽ H v) → (typeOfᴱ H (Γ ⊕ x ↦ T) (var y) ≡ typeOfᴱ H Γ (var y [ v / x ]ᴱwhenever (no p)))
substitutivityᴮ : ∀ {Γ T} H B v x → (just T ≡ typeOfⱽ H v) → (typeOfᴮ H (Γ ⊕ x ↦ T) B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮ))
substitutivityᴮ-unless-yes : ∀ {Γ Γ′ T} H B v x y (p : x ≡ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ) → (typeOfᴮ H Γ′ B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮunless (yes p)))
substitutivityᴮ-unless-no : ∀ {Γ Γ′ T} H B v x y (p : x ≢ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → (typeOfᴮ H Γ′ B ≡ typeOfᴮ H Γ (B [ v / x ]ᴮunless (no p)))

substitutivityᴱ H (var y) v x p with x ≡ⱽ y
substitutivityᴱ H (var y) v x p | yes q = substitutivityᴱ-whenever-yes H v x y q p
substitutivityᴱ H (var y) v x p | no q = substitutivityᴱ-whenever-no H v x y q p
substitutivityᴱ H (val w) v x p = refl
substitutivityᴱ H (binexp M op N) v x p = refl
substitutivityᴱ H (M $ N) v x p = cong tgt (substitutivityᴱ H M v x p)
substitutivityᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p = refl
substitutivityᴱ H (block var b ∈ T is B end) v x p = refl
substitutivityᴱ-whenever-yes H v x x refl q = cong orNone q
substitutivityᴱ-whenever-no H v x y p q = cong orNone ( sym (⊕-lookup-miss x y _ _ p))
substitutivityᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p with x ≡ⱽ f
substitutivityᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p | yes q = substitutivityᴮ-unless-yes H B v x f q p (⊕-over q)
substitutivityᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p | no q = substitutivityᴮ-unless-no H B v x f q p (⊕-swap q)
substitutivityᴮ H (local var y ∈ T ← M ∙ B) v x p with x ≡ⱽ y
substitutivityᴮ H (local var y ∈ T  ← M ∙ B) v x p | yes q = substitutivityᴮ-unless-yes H B v x y q p (⊕-over q)
substitutivityᴮ H (local var y ∈ T  ← M ∙ B) v x p | no q =  substitutivityᴮ-unless-no H B v x y q p (⊕-swap q)
substitutivityᴮ H (return M ∙ B) v x p = substitutivityᴱ H M v x p
substitutivityᴮ H done v x p = refl
substitutivityᴮ-unless-yes H B v x x refl q refl = refl
substitutivityᴮ-unless-no H B v x y p q refl = substitutivityᴮ H B v x q

binOpPreservation : ∀ H {op v w x} → (v ⟦ op ⟧ w ⟶ x) → (tgtBinOp op ≡ typeOfᴱ H ∅ (val x))
binOpPreservation H (+ m n) = refl
binOpPreservation H (- m n) = refl
binOpPreservation H (/ m n) = refl
binOpPreservation H (* m n) = refl
binOpPreservation H (< m n) = refl
binOpPreservation H (> m n) = refl
binOpPreservation H (<= m n) = refl
binOpPreservation H (>= m n) = refl
binOpPreservation H (== v w) = refl
binOpPreservation H (~= v w) = refl

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
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) with remember (typeOfⱽ H v)
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) with S ≡ᵀ U | T ≡ᵀ typeOfᴮ H (x ↦ S) B
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) | yes refl | yes refl = ok (cong tgt (cong orNone (cong typeOfᴹᴼ p)))
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) | yes refl | no r = warning (heap (addr a p (FunctionDefnMismatch r)))
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (just U , q) | no r | _ = warning (expr (FunctionCallMismatch (λ s → r (trans (trans (sym (cong src (cong orNone (cong typeOfᴹᴼ p)))) s) (cong orNone q)))))
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (nothing , q) with typeOf-val-not-none v
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (nothing , q) | ok r = CONTRADICTION (r (sym (cong orNone q)))
preservationᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ S ⟩∈ T is B end) v refl p) | (nothing , q) | warning W = warning (expr (app₂ W))
preservationᴱ H (block var b ∈ T is B end) (block s) = ok refl
preservationᴱ H (block var b ∈ T is return M ∙ B end) (return v) with T ≡ᵀ typeOfᴱ H ∅ (val v)
preservationᴱ H (block var b ∈ T is return M ∙ B end) (return v) | yes p = ok p
preservationᴱ H (block var b ∈ T is return M ∙ B end) (return v) | no p = warning (expr (BlockMismatch p))
preservationᴱ H (block var b ∈ T is done end) (done) with T ≡ᵀ nil
preservationᴱ H (block var b ∈ T is done end) (done) | yes p = ok p
preservationᴱ H (block var b ∈ T is done end) (done) | no p = warning (expr (BlockMismatch p))
preservationᴱ H (binexp M op N) (binOp₀ s) = ok (binOpPreservation H s)
preservationᴱ H (binexp M op N) (binOp₁ s) = ok refl
preservationᴱ H (binexp M op N) (binOp₂ s) = ok refl

preservationᴮ H (local var x ∈ T ← M ∙ B) (local s) with heap-weakeningᴮ H B (rednᴱ⊑ s)
preservationᴮ H (local var x ∈ T ← M ∙ B) (local s) | ok p = ok p
preservationᴮ H (local var x ∈ T ← M ∙ B) (local s) | warning W = warning (block (local₂ W))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) with remember (typeOfⱽ H v)
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (just U , p) with T ≡ᵀ U
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (just T , p) | yes refl = ok (substitutivityᴮ H B v x (sym p))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (just U , p) | no q = warning (block (LocalVarMismatch (λ r → q (trans r (cong orNone p)))))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (nothing , p) with typeOf-val-not-none v
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (nothing , p) | ok q = CONTRADICTION (q (sym (cong orNone p)))
preservationᴮ H (local var x ∈ T ← M ∙ B) (subst v) | (nothing , p) | warning W = warning (block (local₁ W))
preservationᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) with heap-weakeningᴮ H B (snoc defn)
preservationᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) | ok r = ok (trans r (substitutivityᴮ _ B (addr a) f refl))
preservationᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) | warning W = warning (block (function₂ W))
preservationᴮ H (return M ∙ B) (return s) with preservationᴱ H M s
preservationᴮ H (return M ∙ B) (return s) | ok p = ok p
preservationᴮ H (return M ∙ B) (return s) | warning (expr W) = warning (block (return W))
preservationᴮ H (return M ∙ B) (return s) | warning (heap W) = warning (heap W)

reflect-substitutionᴱ : ∀ {Γ T} H M v x → (just T ≡ typeOfⱽ H v) → Warningᴱ H (typeCheckᴱ H Γ (M [ v / x ]ᴱ)) → Warningᴱ H (typeCheckᴱ H (Γ ⊕ x ↦ T) M)
reflect-substitutionᴱ-whenever-yes : ∀ {Γ T} H v x y (p : x ≡ y) → (just T ≡ typeOfⱽ H v) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever yes p)) → Warningᴱ H (typeCheckᴱ H (Γ ⊕ x ↦ T) (var y))
reflect-substitutionᴱ-whenever-no : ∀ {Γ T} H v x y (p : x ≢ y) → (just T ≡ typeOfⱽ H v) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever no p)) → Warningᴱ H (typeCheckᴱ H  (Γ ⊕ x ↦ T) (var y))
reflect-substitutionᴮ : ∀ {Γ T} H B v x → (just T ≡ typeOfⱽ H v) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮ)) → Warningᴮ H (typeCheckᴮ H (Γ ⊕ x ↦ T) B)
reflect-substitutionᴮ-unless-yes : ∀ {Γ Γ′ T} H B v x y (r : x ≡ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless yes r)) → Warningᴮ H (typeCheckᴮ H Γ′ B)
reflect-substitutionᴮ-unless-no : ∀ {Γ Γ′ T} H B v x y (r : x ≢ y) → (just T ≡ typeOfⱽ H v) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless no r)) → Warningᴮ H (typeCheckᴮ H Γ′ B)

reflect-substitutionᴱ H (var y) v x p W with x ≡ⱽ y
reflect-substitutionᴱ H (var y) v x p W | yes r = reflect-substitutionᴱ-whenever-yes H v x y r p W
reflect-substitutionᴱ H (var y) v x p W | no r = reflect-substitutionᴱ-whenever-no H v x y r p W
reflect-substitutionᴱ H (val (addr a)) v x p (UnallocatedAddress r) = UnallocatedAddress r
reflect-substitutionᴱ H (M $ N) v x p (FunctionCallMismatch q) = FunctionCallMismatch (λ s → q (trans (cong src (sym (substitutivityᴱ H M v x p))) (trans s (substitutivityᴱ H N v x p))))
reflect-substitutionᴱ H (M $ N) v x p (app₁ W) = app₁ (reflect-substitutionᴱ H M v x p W)
reflect-substitutionᴱ H (M $ N) v x p (app₂ W) = app₂ (reflect-substitutionᴱ H N v x p W)
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (FunctionDefnMismatch q) with (x ≡ⱽ y)
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (FunctionDefnMismatch q) | yes r = FunctionDefnMismatch (λ s → q (trans s (substitutivityᴮ-unless-yes H B v x y r p (⊕-over r))))
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (FunctionDefnMismatch q) | no r = FunctionDefnMismatch (λ s → q (trans s (substitutivityᴮ-unless-no H B v x y r p (⊕-swap r))))
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₁ W) with (x ≡ⱽ y)
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₁ W) | yes r = function₁ (reflect-substitutionᴮ-unless-yes H B v x y r p (⊕-over r) W)
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p (function₁ W) | no r = function₁ (reflect-substitutionᴮ-unless-no H B v x y r p (⊕-swap r) W)
reflect-substitutionᴱ H (block var b ∈ T is B end) v x p (BlockMismatch q) = BlockMismatch (λ r → q (trans r (substitutivityᴮ H B v x p)))
reflect-substitutionᴱ H (block var b ∈ T is B end) v x p (block₁ W) = block₁ (reflect-substitutionᴮ H B v x p W)
reflect-substitutionᴱ H (binexp M op N) x v p (BinOpMismatch₁ q) = BinOpMismatch₁ (subst₁ (BinOpWarning op) (sym (substitutivityᴱ H M x v p)) q)
reflect-substitutionᴱ H (binexp M op N) x v p (BinOpMismatch₂ q) = BinOpMismatch₂ (subst₁ (BinOpWarning op) (sym (substitutivityᴱ H N x v p)) q)
reflect-substitutionᴱ H (binexp M op N) x v p (bin₁ W) = bin₁ (reflect-substitutionᴱ H M x v p W)
reflect-substitutionᴱ H (binexp M op N) x v p (bin₂ W) = bin₂ (reflect-substitutionᴱ H N x v p W)

reflect-substitutionᴱ-whenever-no H v x y p q (UnboundVariable r) = UnboundVariable (trans (sym (⊕-lookup-miss x y _ _ p)) r)
reflect-substitutionᴱ-whenever-yes H (addr a) x x refl p (UnallocatedAddress q) with trans p (cong typeOfᴹᴼ q)
reflect-substitutionᴱ-whenever-yes H (addr a) x x refl p (UnallocatedAddress q) | ()

reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (FunctionDefnMismatch q) with (x ≡ⱽ y)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (FunctionDefnMismatch q) | yes r = FunctionDefnMismatch (λ s → q (trans s (substitutivityᴮ-unless-yes H C v x y r p (⊕-over r))))
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (FunctionDefnMismatch q) | no r = FunctionDefnMismatch (λ s → q (trans s (substitutivityᴮ-unless-no H C v x y r p (⊕-swap r))))
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₁ W) with (x ≡ⱽ y)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₁ W) | yes r = function₁ (reflect-substitutionᴮ-unless-yes H C v x y r p (⊕-over r) W)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₁ W) | no r = function₁ (reflect-substitutionᴮ-unless-no H C v x y r p (⊕-swap r) W)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₂ W) with (x ≡ⱽ f)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₂ W)| yes r = function₂ (reflect-substitutionᴮ-unless-yes H B v x f r p (⊕-over r) W)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p (function₂ W)| no r = function₂ (reflect-substitutionᴮ-unless-no H B v x f r p (⊕-swap r) W)
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x p (LocalVarMismatch q) = LocalVarMismatch (λ r → q (trans r (substitutivityᴱ H M v x p)))
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x p (local₁ W) = local₁ (reflect-substitutionᴱ H M v x p W)
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x p (local₂ W) with (x ≡ⱽ y)
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x p (local₂ W) | yes r = local₂ (reflect-substitutionᴮ-unless-yes H B v x y r p (⊕-over r) W)
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x p (local₂ W) | no r = local₂ (reflect-substitutionᴮ-unless-no H B v x y r p (⊕-swap r) W)
reflect-substitutionᴮ H (return M ∙ B) v x p (return W) = return (reflect-substitutionᴱ H M v x p W)

reflect-substitutionᴮ-unless-yes H B v x y r p refl W = W
reflect-substitutionᴮ-unless-no H B v x y r p refl W = reflect-substitutionᴮ H B v x p W

reflect-weakeningᴱ : ∀ H M {H′ Γ} → (H ⊑ H′) → Warningᴱ H′ (typeCheckᴱ H′ Γ M) → Warningᴱ H (typeCheckᴱ H Γ M)
reflect-weakeningᴮ : ∀ H B {H′ Γ} → (H ⊑ H′) → Warningᴮ H′ (typeCheckᴮ H′ Γ B) → Warningᴮ H (typeCheckᴮ H Γ B)

reflect-weakeningᴱ H (var x) h (UnboundVariable p) = (UnboundVariable p)
reflect-weakeningᴱ H (val (addr a)) h (UnallocatedAddress p) = UnallocatedAddress (lookup-⊑-nothing a h p)
reflect-weakeningᴱ H (M $ N) h (FunctionCallMismatch p) with heap-weakeningᴱ H M h | heap-weakeningᴱ H N h
reflect-weakeningᴱ H (M $ N) h (FunctionCallMismatch p) | ok q₁ | ok q₂ = FunctionCallMismatch (λ r → p (trans (cong src (sym q₁)) (trans r q₂)))
reflect-weakeningᴱ H (M $ N) h (FunctionCallMismatch p) | warning W | _ = app₁ W
reflect-weakeningᴱ H (M $ N) h (FunctionCallMismatch p) | _ | warning W = app₂ W
reflect-weakeningᴱ H (M $ N) h (app₁ W) = app₁ (reflect-weakeningᴱ H M h W)
reflect-weakeningᴱ H (M $ N) h (app₂ W) = app₂ (reflect-weakeningᴱ H N h W)
reflect-weakeningᴱ H (binexp M op N) h (BinOpMismatch₁ p) with heap-weakeningᴱ H M h
reflect-weakeningᴱ H (binexp M op N) h (BinOpMismatch₁ p) | ok q = BinOpMismatch₁ (subst₁ (BinOpWarning op) (sym q) p)
reflect-weakeningᴱ H (binexp M op N) h (BinOpMismatch₁ p) | warning W = bin₁ W
reflect-weakeningᴱ H (binexp M op N) h (BinOpMismatch₂ p) with heap-weakeningᴱ H N h
reflect-weakeningᴱ H (binexp M op N) h (BinOpMismatch₂ p) | ok q = BinOpMismatch₂ (subst₁ (BinOpWarning op) (sym q) p)
reflect-weakeningᴱ H (binexp M op N) h (BinOpMismatch₂ p) | warning W = bin₂ W
reflect-weakeningᴱ H (binexp M op N) h (bin₁ W′) = bin₁ (reflect-weakeningᴱ H M h W′)
reflect-weakeningᴱ H (binexp M op N) h (bin₂ W′) = bin₂ (reflect-weakeningᴱ H N h W′)
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (FunctionDefnMismatch p) with heap-weakeningᴮ H B h
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (FunctionDefnMismatch p) | ok q = FunctionDefnMismatch (λ r → p (trans r q))
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (FunctionDefnMismatch p) | warning W = function₁ W
reflect-weakeningᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (function₁ W) = function₁ (reflect-weakeningᴮ H B h W)
reflect-weakeningᴱ H (block var b ∈ T is B end) h (BlockMismatch p) with heap-weakeningᴮ H B h
reflect-weakeningᴱ H (block var b ∈ T is B end) h (BlockMismatch p) | ok q = BlockMismatch (λ r → p (trans r q))
reflect-weakeningᴱ H (block var b ∈ T is B end) h (BlockMismatch p) | warning W = block₁ W
reflect-weakeningᴱ H (block var b ∈ T is B end) h (block₁ W) = block₁ (reflect-weakeningᴮ H B h W)

reflect-weakeningᴮ H (return M ∙ B) h (return W) = return (reflect-weakeningᴱ H M h W)
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (LocalVarMismatch p) with heap-weakeningᴱ H M h
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (LocalVarMismatch p) | ok q = LocalVarMismatch (λ r → p (trans r q))
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (LocalVarMismatch p) | warning W = local₁ W
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (local₁ W) = local₁ (reflect-weakeningᴱ H M h W)
reflect-weakeningᴮ H (local var y ∈ T ← M ∙ B) h (local₂ W) = local₂ (reflect-weakeningᴮ H B h W)
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (FunctionDefnMismatch p) with heap-weakeningᴮ H C h
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (FunctionDefnMismatch p) | ok q = FunctionDefnMismatch (λ r → p (trans r q))
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (FunctionDefnMismatch p) | warning W = function₁ W
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₁ W) = function₁ (reflect-weakeningᴮ H C h W)
reflect-weakeningᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₂ W) = function₂ (reflect-weakeningᴮ H B h W)

reflect-weakeningᴼ : ∀ H O {H′} → (H ⊑ H′) → Warningᴼ H′ (typeCheckᴼ H′ O) → Warningᴼ H (typeCheckᴼ H O)
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (FunctionDefnMismatch p) with heap-weakeningᴮ H B h
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (FunctionDefnMismatch p) | ok q = FunctionDefnMismatch (λ r → p (trans r q))
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (FunctionDefnMismatch p) | warning W = function₁ W
reflect-weakeningᴼ H (just (function f ⟨ var x ∈ T ⟩∈ U is B end)) h (function₁ W′) = function₁ (reflect-weakeningᴮ H B h W′)

reflectᴱ : ∀ H M {H′ M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴱ H′ (typeCheckᴱ H′ ∅ M′) → Warningᴴᴱ H (typeCheckᴴᴱ H ∅ M)
reflectᴮ : ∀ H B {H′ B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴮ H′ (typeCheckᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)

reflectᴱ H (M $ N) (app₁ s) (FunctionCallMismatch p) with preservationᴱ H M s | heap-weakeningᴱ H N (rednᴱ⊑ s)
reflectᴱ H (M $ N) (app₁ s) (FunctionCallMismatch p) | ok q | ok q′ = expr (FunctionCallMismatch (λ r → p (trans (trans (cong src (sym q)) r) q′)))
reflectᴱ H (M $ N) (app₁ s) (FunctionCallMismatch p) | warning (expr W) | _ = expr (app₁ W)
reflectᴱ H (M $ N) (app₁ s) (FunctionCallMismatch p) | warning (heap W) | _ = heap W
reflectᴱ H (M $ N) (app₁ s) (FunctionCallMismatch p) | _ | warning W  = expr (app₂ W)
reflectᴱ H (M $ N) (app₁ s) (app₁ W′) with reflectᴱ H M s W′
reflectᴱ H (M $ N) (app₁ s) (app₁ W′) | heap W = heap W
reflectᴱ H (M $ N) (app₁ s) (app₁ W′) | expr W = expr (app₁ W)
reflectᴱ H (M $ N) (app₁ s) (app₂ W′) = expr (app₂ (reflect-weakeningᴱ H N (rednᴱ⊑ s) W′))
reflectᴱ H (M $ N) (app₂ p s) (FunctionCallMismatch p′) with heap-weakeningᴱ H (val p) (rednᴱ⊑ s) | preservationᴱ H N s
reflectᴱ H (M $ N) (app₂ p s) (FunctionCallMismatch p′) | ok q | ok q′ = expr (FunctionCallMismatch (λ r → p′ (trans (trans (cong src (sym q)) r) q′)))
reflectᴱ H (M $ N) (app₂ p s) (FunctionCallMismatch p′) | warning W | _ = expr (app₁ W)
reflectᴱ H (M $ N) (app₂ p s) (FunctionCallMismatch p′) | _ | warning (expr W) = expr (app₂ W)
reflectᴱ H (M $ N) (app₂ p s) (FunctionCallMismatch p′) | _ | warning (heap W) = heap W
reflectᴱ H (M $ N) (app₂ p s) (app₁ W′) = expr (app₁ (reflect-weakeningᴱ H M (rednᴱ⊑ s) W′))
reflectᴱ H (M $ N) (app₂ p s) (app₂ W′) with reflectᴱ H N s W′
reflectᴱ H (M $ N) (app₂ p s) (app₂ W′) | heap W = heap W
reflectᴱ H (M $ N) (app₂ p s) (app₂ W′) | expr W = expr (app₂ W)
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) with remember (typeOfⱽ H v)
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | (just S , r) with S ≡ᵀ T
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | (just T , r) | yes refl = heap (addr a p (FunctionDefnMismatch (λ s → q (trans s (substitutivityᴮ H B v x (sym r))))))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | (just S , r) | no s = expr (FunctionCallMismatch (λ t → s (trans (cong orNone (sym r)) (trans (sym t) (cong src (cong orNone (cong typeOfᴹᴼ p)))))))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | (nothing , r) with typeOf-val-not-none v
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | (nothing , r) | ok s = CONTRADICTION (s (cong orNone (sym r)))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | (nothing , r) | warning W = expr (app₂ W)
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) with remember (typeOfⱽ H v)
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | (just S , q) with S ≡ᵀ T
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | (just T , q) | yes refl = heap (addr a p (function₁ (reflect-substitutionᴮ H B v x (sym q) W′)))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | (just S , q) | no r = expr (FunctionCallMismatch (λ s → r (trans (cong orNone (sym q)) (trans (sym s) (cong src (cong orNone (cong typeOfᴹᴼ p)))))))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | (nothing , q) with typeOf-val-not-none v
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | (nothing , q) | ok r = CONTRADICTION (r (cong orNone (sym q)))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | (nothing , q) | warning W = expr (app₂ W)
reflectᴱ H (block var b ∈ T is B end) (block s) (BlockMismatch p) with preservationᴮ H B s
reflectᴱ H (block var b ∈ T is B end) (block s) (BlockMismatch p) | ok q = expr (BlockMismatch (λ r → p (trans r q)))
reflectᴱ H (block var b ∈ T is B end) (block s) (BlockMismatch p) | warning (heap W) = heap W
reflectᴱ H (block var b ∈ T is B end) (block s) (BlockMismatch p) | warning (block W) = expr (block₁ W)
reflectᴱ H (block var b ∈ T is B end) (block s) (block₁ W′) with reflectᴮ H B s W′
reflectᴱ H (block var b ∈ T is B end) (block s) (block₁ W′) | heap W = heap W
reflectᴱ H (block var b ∈ T is B end) (block s) (block₁ W′) | block W = expr (block₁ W)
reflectᴱ H (block var b ∈ T is B end) (return v) W′ = expr (block₁ (return W′))
reflectᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (UnallocatedAddress ())
reflectᴱ H (binexp M op N) (binOp₀ ()) (UnallocatedAddress p)
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₁ p) with preservationᴱ H M s
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₁ p) | ok q = expr (BinOpMismatch₁ (subst₁ (BinOpWarning op) (sym q) p))
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₁ p) | warning (heap W) = heap W
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₁ p) | warning (expr W) = expr (bin₁ W)
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₂ p) with heap-weakeningᴱ H N (rednᴱ⊑ s)
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₂ p) | ok q = expr (BinOpMismatch₂ ((subst₁ (BinOpWarning op) (sym q) p)))
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₂ p) | warning W = expr (bin₂ W)
reflectᴱ H (binexp M op N) (binOp₁ s) (bin₁ W′) with reflectᴱ H M s W′
reflectᴱ H (binexp M op N) (binOp₁ s) (bin₁ W′) | heap W = heap W
reflectᴱ H (binexp M op N) (binOp₁ s) (bin₁ W′) | expr W = expr (bin₁ W)
reflectᴱ H (binexp M op N) (binOp₁ s) (bin₂ W′) = expr (bin₂ (reflect-weakeningᴱ H N (rednᴱ⊑ s) W′))
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₁ p) with heap-weakeningᴱ H M (rednᴱ⊑ s)
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₁ p) | ok q = expr (BinOpMismatch₁ (subst₁ (BinOpWarning op) (sym q) p))
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₁ p) | warning W = expr (bin₁ W)
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₂ p) with preservationᴱ H N s
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₂ p) | ok q = expr (BinOpMismatch₂ (subst₁ (BinOpWarning op) (sym q) p))
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₂ p) | warning (heap W) = heap W
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₂ p) | warning (expr W) = expr (bin₂ W)
reflectᴱ H (binexp M op N) (binOp₂ s) (bin₁ W′) = expr (bin₁ (reflect-weakeningᴱ H M (rednᴱ⊑ s) W′))
reflectᴱ H (binexp M op N) (binOp₂ s) (bin₂ W′) with reflectᴱ H N s W′
reflectᴱ H (binexp M op N) (binOp₂ s) (bin₂ W′) | heap W = heap W
reflectᴱ H (binexp M op N) (binOp₂ s) (bin₂ W′) | expr W = expr (bin₂ W)

reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (LocalVarMismatch p) with preservationᴱ H M s
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (LocalVarMismatch p) | ok q = block (LocalVarMismatch (λ r → p (trans r q)))
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (LocalVarMismatch p) | warning (expr W) = block (local₁ W)
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (LocalVarMismatch p) | warning (heap W) = heap W
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₁ W′) with reflectᴱ H M s W′
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₁ W′) | heap W = heap W
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₁ W′) | expr W = block (local₁ W)
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₂ W′) = block (local₂ (reflect-weakeningᴮ H B (rednᴱ⊑ s) W′))
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ with remember (typeOfⱽ H v)
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ | (just S , p) with S ≡ᵀ T
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ | (just T , p) | yes refl = block (local₂ (reflect-substitutionᴮ H B v x (sym p) W′))
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ | (just S , p) | no q = block (LocalVarMismatch (λ r → q (trans (cong orNone (sym p)) (sym r))))
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ | (nothing , p) with typeOf-val-not-none v
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ | (nothing , p) | ok r = CONTRADICTION (r (cong orNone (sym p)))
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ | (nothing , p) | warning W = block (local₁ W)
reflectᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) W′ = block (function₂ (reflect-weakeningᴮ H B (snoc defn) (reflect-substitutionᴮ _ B (addr a) f refl W′)))
reflectᴮ H (return M ∙ B) (return s) (return W′) with reflectᴱ H M s W′
reflectᴮ H (return M ∙ B) (return s) (return W′) | heap W = heap W
reflectᴮ H (return M ∙ B) (return s) (return W′) | expr W = block (return W)

reflectᴴᴱ : ∀ H M {H′ M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴴᴱ H′ (typeCheckᴴᴱ H′ ∅ M′) → Warningᴴᴱ H (typeCheckᴴᴱ H ∅ M)
reflectᴴᴮ : ∀ H B {H′ B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)

reflectᴴᴱ H M s (expr W′) = reflectᴱ H M s W′
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a p) (heap (addr b refl W′)) with b ≡ᴬ a
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (FunctionDefnMismatch p))) | yes refl with heap-weakeningᴮ H B (snoc defn)
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (FunctionDefnMismatch p))) | yes refl | ok r = expr (FunctionDefnMismatch λ q → p (trans q r))
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (FunctionDefnMismatch p))) | yes refl | warning W = expr (function₁ W)
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (heap (addr a refl (function₁ W′))) | yes refl = expr (function₁ (reflect-weakeningᴮ H B (snoc defn) W′))
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
reflectᴴᴱ H (block var b ∈ T is B end) (block s) (heap W′) | block W = expr (block₁ W)
reflectᴴᴱ H (block var b ∈ T is return N ∙ B end) (return v) (heap W′) = heap W′
reflectᴴᴱ H (block var b ∈ T is done end) done (heap W′) = heap W′
reflectᴴᴱ H (binexp M op N) (binOp₀ s) (heap W′) = heap W′
reflectᴴᴱ H (binexp M op N) (binOp₁ s) (heap W′) with reflectᴴᴱ H M s (heap W′)
reflectᴴᴱ H (binexp M op N) (binOp₁ s) (heap W′) | heap W = heap W
reflectᴴᴱ H (binexp M op N) (binOp₁ s) (heap W′) | expr W = expr (bin₁ W)
reflectᴴᴱ H (binexp M op N) (binOp₂ s) (heap W′) with reflectᴴᴱ H N s (heap W′)
reflectᴴᴱ H (binexp M op N) (binOp₂ s) (heap W′) | heap W = heap W
reflectᴴᴱ H (binexp M op N) (binOp₂ s) (heap W′) | expr W = expr (bin₂ W)

reflectᴴᴮ H B s (block W′) = reflectᴮ H B s W′
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (local s) (heap W′) with reflectᴴᴱ H M s (heap W′)
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (local s) (heap W′) | heap W = heap W
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (local s) (heap W′) | expr W = block (local₁ W)
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (subst v) (heap W′) = heap W′
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a p) (heap (addr b refl W′)) with b ≡ᴬ a
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (FunctionDefnMismatch p))) | yes refl with heap-weakeningᴮ H C (snoc defn)
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (FunctionDefnMismatch p))) | yes refl | ok r = block (FunctionDefnMismatch (λ q → p (trans q r)))
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (FunctionDefnMismatch p))) | yes refl | warning W = block (function₁ W)
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) (heap (addr a refl (function₁ W′))) | yes refl = block (function₁ (reflect-weakeningᴮ H C (snoc defn) W′))
reflectᴴᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a p) (heap (addr b refl W′)) | no r = heap (addr b (lookup-not-allocated p r) (reflect-weakeningᴼ H _ (snoc p) W′))
reflectᴴᴮ H (return M ∙ B) (return s) (heap W′) with reflectᴴᴱ H M s (heap W′)
reflectᴴᴮ H (return M ∙ B) (return s) (heap W′) | heap W = heap W
reflectᴴᴮ H (return M ∙ B) (return s) (heap W′) | expr W = block (return W)

reflect* : ∀ H B {H′ B′} → (H ⊢ B ⟶* B′ ⊣ H′) → Warningᴴᴮ H′ (typeCheckᴴᴮ H′ ∅ B′) → Warningᴴᴮ H (typeCheckᴴᴮ H ∅ B)
reflect* H B refl W = W
reflect* H B (step s t) W = reflectᴴᴮ H B s (reflect* _ _ t W)

runtimeBinOpWarning : ∀ H {op} v → BinOpError op (valueType v) → BinOpWarning op (orNone (typeOfⱽ H v))
runtimeBinOpWarning H v (+ p) = + (λ q → p (mustBeNumber H ∅ v q))
runtimeBinOpWarning H v (- p) = - (λ q → p (mustBeNumber H ∅ v q))
runtimeBinOpWarning H v (* p) = * (λ q → p (mustBeNumber H ∅ v q))
runtimeBinOpWarning H v (/ p) = / (λ q → p (mustBeNumber H ∅ v q))
runtimeBinOpWarning H v (< p) = < (λ q → p (mustBeNumber H ∅ v q))
runtimeBinOpWarning H v (> p) = > (λ q → p (mustBeNumber H ∅ v q))
runtimeBinOpWarning H v (<= p) = <= (λ q → p (mustBeNumber H ∅ v q))
runtimeBinOpWarning H v (>= p) = >= (λ q → p (mustBeNumber H ∅ v q))

runtimeWarningᴱ : ∀ H M → RuntimeErrorᴱ H M → Warningᴱ H (typeCheckᴱ H ∅ M)
runtimeWarningᴮ : ∀ H B → RuntimeErrorᴮ H B → Warningᴮ H (typeCheckᴮ H ∅ B)

runtimeWarningᴱ H (var x) UnboundVariable = UnboundVariable refl
runtimeWarningᴱ H (val (addr a)) (SEGV p) = UnallocatedAddress p
runtimeWarningᴱ H (M $ N) (FunctionMismatch v w p) with typeOf-val-not-none w
runtimeWarningᴱ H (M $ N) (FunctionMismatch v w p) | ok q = FunctionCallMismatch (λ r → p (mustBeFunction H ∅ v (λ r′ → q (trans r′ r))))
runtimeWarningᴱ H (M $ N) (FunctionMismatch v w p) | warning W = app₂ W
runtimeWarningᴱ H (M $ N) (app₁ err) = app₁ (runtimeWarningᴱ H M err)
runtimeWarningᴱ H (M $ N) (app₂ err) = app₂ (runtimeWarningᴱ H N err)
runtimeWarningᴱ H (block var b ∈ T is B end) (block err) = block₁ (runtimeWarningᴮ H B err)
runtimeWarningᴱ H (binexp M op N) (BinOpMismatch₁ v w p) = BinOpMismatch₁ (runtimeBinOpWarning H v p)
runtimeWarningᴱ H (binexp M op N) (BinOpMismatch₂ v w p) = BinOpMismatch₂ (runtimeBinOpWarning H w p)
runtimeWarningᴱ H (binexp M op N) (bin₁ err) = bin₁ (runtimeWarningᴱ H M err)
runtimeWarningᴱ H (binexp M op N) (bin₂ err) = bin₂ (runtimeWarningᴱ H N err)

runtimeWarningᴮ H (local var x ∈ T ← M ∙ B) (local err) = local₁ (runtimeWarningᴱ H M err)
runtimeWarningᴮ H (return M ∙ B) (return err) = return (runtimeWarningᴱ H M err)

wellTypedProgramsDontGoWrong : ∀ H′ B B′ → (∅ᴴ ⊢ B ⟶* B′ ⊣ H′) → (RuntimeErrorᴮ H′ B′) → Warningᴮ ∅ᴴ (typeCheckᴮ ∅ᴴ ∅ B)
wellTypedProgramsDontGoWrong H′ B B′ t err with reflect* ∅ᴴ B t (block (runtimeWarningᴮ H′ B′ err))
wellTypedProgramsDontGoWrong H′ B B′ t err | heap (addr a refl ())
wellTypedProgramsDontGoWrong H′ B B′ t err | block W = W
