{-# OPTIONS --rewriting #-}

module Properties.StrictMode where

import Agda.Builtin.Equality.Rewrite
open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Heap using (Heap; Object; function_is_end; defn; alloc; ok; next; lookup-not-allocated) renaming (_≡_⊕_↦_ to _≡ᴴ_⊕_↦_; _[_] to _[_]ᴴ; ∅ to ∅ᴴ)
open import Luau.StrictMode using (Warningᴱ; Warningᴮ; Warningᴼ; Warningᴴ; UnallocatedAddress; UnboundVariable; FunctionCallMismatch; app₁; app₂; BinOpMismatch₁; BinOpMismatch₂; bin₁; bin₂; BlockMismatch; block₁; return; LocalVarMismatch; local₁; local₂; FunctionDefnMismatch; function₁; function₂; heap; expr; block; addr; _≮:_; witness; any; none; nil; number; string; boolean; scalar; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-err; left; right; _,_; Tree; Language; ¬Language; Scalar)
open import Luau.Substitution using (_[_/_]ᴮ; _[_/_]ᴱ; _[_/_]ᴮunless_; var_[_/_]ᴱwhenever_)
open import Luau.Syntax using (Expr; yes; var; val; var_∈_; _⟨_⟩∈_; _$_; addr; number; bool; string; binexp; nil; function_is_end; block_is_end; done; return; local_←_; _∙_; fun; arg; name; ==; ~=)
open import Luau.Type using (Type; strict; nil; number; boolean; string; _⇒_; none; any; _∩_; _∪_; tgt; _≡ᵀ_; _≡ᴹᵀ_)
open import Luau.TypeCheck(strict) using (_⊢ᴮ_∈_; _⊢ᴱ_∈_; _⊢ᴴᴮ_▷_∈_; _⊢ᴴᴱ_▷_∈_; nil; var; addr; app; function; block; done; return; local; orAny; srcBinOp; tgtBinOp)
open import Luau.Var using (_≡ⱽ_)
open import Luau.Addr using (_≡ᴬ_)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_; ⊕-lookup-miss; ⊕-swap; ⊕-over) renaming (_[_] to _[_]ⱽ)
open import Luau.VarCtxt using (VarCtxt; ∅)
open import Properties.Remember using (remember; _,_)
open import Properties.Equality using (_≢_; sym; cong; trans; subst₁)
open import Properties.Dec using (Dec; yes; no)
open import Properties.Contradiction using (CONTRADICTION; ¬)
open import Properties.TypeCheck(strict) using (typeOfᴼ; typeOfᴹᴼ; typeOfⱽ; typeOfᴱ; typeOfᴮ; typeCheckᴱ; typeCheckᴮ; typeCheckᴼ; typeCheckᴴ)
open import Luau.OpSem using (_⟦_⟧_⟶_; _⊢_⟶*_⊣_; _⊢_⟶ᴮ_⊣_; _⊢_⟶ᴱ_⊣_; app₁; app₂; function; beta; return; block; done; local; subst; binOp₀; binOp₁; binOp₂; refl; step; +; -; *; /; <; >; ==; ~=; <=; >=; ··)
open import Luau.RuntimeError using (BinOpError; RuntimeErrorᴱ; RuntimeErrorᴮ; FunctionMismatch; BinOpMismatch₁; BinOpMismatch₂; UnboundVariable; SEGV; app₁; app₂; bin₁; bin₂; block; local; return; +; -; *; /; <; >; <=; >=; ··)
open import Luau.RuntimeType using (RuntimeType; valueType; number; string; boolean; nil; function)

-- Move these! --
swapLR : ∀ {A B} → Either A B → Either B A
swapLR (Left x) = Right x
swapLR (Right x) = Left x

mapL : ∀ {A B C} → (A → B) → Either A C → Either B C
mapL f (Left x) = Left (f x)
mapL f (Right x) = Right x

mapR : ∀ {A B C} → (B → C) → Either A B → Either A C
mapR f (Left x) = Left x
mapR f (Right x) = Right (f x)

mapLR : ∀ {A B C D} → (A → B) → (C → D) → Either A C → Either B D
mapLR f g (Left x) = Left (f x)
mapLR f g (Right x) = Right (g x)

cond : ∀ {A B C : Set} → (A → C) → (B → C) → (Either A B) → C
cond f g (Left x) = f x
cond f g (Right x) = g x

infixr 5 _∘_
_∘_ : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
(f ∘ g) x = f (g x)
--

src = Luau.Type.src strict

data _⊑_ (H : Heap yes) : Heap yes → Set where
  refl : (H ⊑ H)
  snoc : ∀ {H′ a O} → (H′ ≡ᴴ H ⊕ a ↦ O) → (H ⊑ H′)

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

dec-language : ∀ T t → Either (¬Language T t) (Language T t)
dec-language = {!!}

≮:-antirefl : ∀ {T} → ¬(T ≮: T)
≮:-antirefl (witness (scalar s) (scalar s) (scalar-scalar s t p)) = CONTRADICTION (p refl)
≮:-antirefl (witness (function-ok t) (function-ok p) (function-ok q)) = ≮:-antirefl (witness t p q)
≮:-antirefl (witness (function-err t) (function-err p) (function-err q)) = ≮:-antirefl (witness t q p)
≮:-antirefl (witness t (left p) (q₁ , q₂)) = ≮:-antirefl (witness t p q₁)
≮:-antirefl (witness t (right p) (q₁ , q₂)) = ≮:-antirefl (witness t p q₂)
≮:-antirefl (witness t (p₁ , p₂) (left q)) = ≮:-antirefl (witness t p₁ q)
≮:-antirefl (witness t (p₁ , p₂) (right q)) = ≮:-antirefl (witness t p₂ q)
≮:-antirefl (witness (scalar s) any (scalar-scalar s () t))
≮:-antirefl (witness (function-ok t) any (scalar-function-ok ()))
≮:-antirefl (witness (function-err t) any (scalar-function-err ()))

≮:-antitrans : ∀ {S T U} → (S ≮: U) → Either (S ≮: T) (T ≮: U)
≮:-antitrans {T = T} (witness t p q) = mapLR (witness t p) (λ z → witness t z q) (dec-language T t)

any-≮: : ∀ {T U} → (T ≮: U) → (any ≮: U)
any-≮: (witness t p q) = witness t any q

none-≮: : ∀ {T U} → (T ≮: U) → (T ≮: none)
none-≮: (witness t p q) = witness t p none

skalar = number ∪ (string ∪ (nil ∪ boolean))

tgt-function-ok : ∀ {T t} → (Language (tgt T) t) → Language T (function-ok t)
tgt-function-ok {T = nil} (scalar ())
tgt-function-ok {T = T₁ ⇒ T₂} p = function-ok p
tgt-function-ok {T = none} (scalar ())
tgt-function-ok {T = any} p = any
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
function-ok-tgt any = any

skalar-function-ok : ∀ {t} → (¬Language skalar (function-ok t))
skalar-function-ok = (scalar-function-ok number , (scalar-function-ok string , (scalar-function-ok nil , scalar-function-ok boolean)))

skalar-scalar : ∀ {T} (s : Scalar T) → (Language skalar (scalar s))
skalar-scalar number = left (scalar number)
skalar-scalar boolean = right (right (right (scalar boolean)))
skalar-scalar string = right (left (scalar string))
skalar-scalar nil = right (right (left (scalar nil)))

tgt-src-≮: : ∀ {T U} → (tgt T ≮: U) → (T ≮: (skalar ∪ (none ⇒ U)))
tgt-src-≮: (witness t p q) = witness (function-ok t) (tgt-function-ok p) (skalar-function-ok , function-ok q)

src-tgt-≮: : ∀ {T U} → (T ≮: (skalar ∪ (none ⇒ U))) → (tgt T ≮: U)
src-tgt-≮: (witness (scalar s) p (q₁ , q₂)) = CONTRADICTION (≮:-antirefl (witness (scalar s) (skalar-scalar s) q₁))
src-tgt-≮: (witness (function-ok t) p (q₁ , function-ok q₂)) = witness t (function-ok-tgt p) q₂
src-tgt-≮: (witness (function-err (scalar s)) p (q₁ , function-err (scalar ())))

src-≮: : ∀ {T U} → (src T ≮: src U) → (U ≮: T)
src-≮: = {!!}

any-src-≮: : ∀ {T U} → (T ≮: (U ⇒ any)) → (U ≮: src T)
any-src-≮: = {!!}

function-≮:-scalar : ∀ {S T U} → (Scalar U) → ((S ⇒ T) ≮: U)
function-≮:-scalar = {!!}

scalar-≮:-function : ∀ {S T U} → (Scalar U) → (U ≮: (S ⇒ T))
scalar-≮:-function s = witness (scalar s) (scalar s) (function-scalar s)

any-≮:-scalar : ∀ {U} → (Scalar U) → (any ≮: U)
any-≮:-scalar s = witness (function-ok (scalar s)) any (scalar-function-ok s)

scalar-≢-impl-≮: : ∀ {T U} → (Scalar T) → (Scalar U) → (T ≢ U) → (T ≮: U)
scalar-≢-impl-≮: s₁ s₂ p = witness (scalar s₁) (scalar s₁) (scalar-scalar s₁ s₂ p)

-- The rest of the proof just depends on those properties

≮:-trans-≡ : ∀ {S T U} → (S ≮: T) → (T ≡ U) → (S ≮: U)
≮:-trans-≡ p refl = p

≡-trans-≮: : ∀ {S T U} → (S ≡ T) → (T ≮: U) → (S ≮: U)
≡-trans-≮: refl p = p

heap-weakeningᴱ : ∀ Γ H M {H′ U} → (H ⊑ H′) → (typeOfᴱ H′ Γ M ≮: U) → (typeOfᴱ H Γ M ≮: U)
heap-weakeningᴱ Γ H (var x) h p = p
heap-weakeningᴱ Γ H (val nil) h p = p
heap-weakeningᴱ Γ H (val (addr a)) refl p = p
heap-weakeningᴱ Γ H (val (addr a)) (snoc {a = b} q) p with a ≡ᴬ b
heap-weakeningᴱ Γ H (val (addr a)) (snoc {a = a} defn) p | yes refl = any-≮: p
heap-weakeningᴱ Γ H (val (addr a)) (snoc {a = b} q) p | no r = ≡-trans-≮: (cong orAny (cong typeOfᴹᴼ (lookup-not-allocated q r))) p
heap-weakeningᴱ Γ H (val (number x)) h p = p
heap-weakeningᴱ Γ H (val (bool x)) h p = p
heap-weakeningᴱ Γ H (val (string x)) h p = p
heap-weakeningᴱ Γ H (M $ N) h p = src-tgt-≮: (heap-weakeningᴱ Γ H M h (tgt-src-≮: p))
heap-weakeningᴱ Γ H (function f ⟨ var x ∈ T ⟩∈ U is B end) h p = p
heap-weakeningᴱ Γ H (block var b ∈ T is B end) h p = p
heap-weakeningᴱ Γ H (binexp M op N) h p = p

heap-weakeningᴮ : ∀ Γ H B {H′ U} → (H ⊑ H′) → (typeOfᴮ H′ Γ B ≮: U) → (typeOfᴮ H Γ B ≮: U)
heap-weakeningᴮ Γ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h p = heap-weakeningᴮ (Γ ⊕ f ↦ (T ⇒ U)) H B h p
heap-weakeningᴮ Γ H (local var x ∈ T ← M ∙ B) h p = heap-weakeningᴮ (Γ ⊕ x ↦ T) H B h p
heap-weakeningᴮ Γ H (return M ∙ B) h p = heap-weakeningᴱ Γ H M h p
heap-weakeningᴮ Γ H done h p = p

substitutivityᴱ : ∀ {Γ T U} H M v x → (typeOfᴱ H Γ (M [ v / x ]ᴱ) ≮: U) → Either (typeOfᴱ H (Γ ⊕ x ↦ T) M ≮: U) (typeOfᴱ H ∅ (val v) ≮: T)
substitutivityᴱ-whenever : ∀ {Γ T U} H v x y (r : Dec(x ≡ y)) → (typeOfᴱ H Γ (var y [ v / x ]ᴱwhenever r) ≮: U) → Either (typeOfᴱ H (Γ ⊕ x ↦ T) (var y) ≮: U) (typeOfᴱ H ∅ (val v) ≮: T)
substitutivityᴮ : ∀ {Γ T U} H B v x → (typeOfᴮ H Γ (B [ v / x ]ᴮ) ≮: U) → Either (typeOfᴮ H (Γ ⊕ x ↦ T) B ≮: U) (typeOfᴱ H ∅ (val v) ≮: T)
substitutivityᴮ-unless : ∀ {Γ T U V} H B v x y (r : Dec(x ≡ y)) → (typeOfᴮ H (Γ ⊕ y ↦ U) (B [ v / x ]ᴮunless r) ≮: V) → Either (typeOfᴮ H ((Γ ⊕ x ↦ T) ⊕ y ↦ U) B ≮: V) (typeOfᴱ H ∅ (val v) ≮: T)
substitutivityᴮ-unless-yes : ∀ {Γ Γ′ T V} H B v x y (r : x ≡ y) → (Γ′ ≡ Γ) → (typeOfᴮ H Γ (B [ v / x ]ᴮunless yes r) ≮: V) → Either (typeOfᴮ H Γ′ B ≮: V) (typeOfᴱ H ∅ (val v) ≮: T)
substitutivityᴮ-unless-no : ∀ {Γ Γ′ T V} H B v x y (r : x ≢ y) → (Γ′ ≡ Γ ⊕ x ↦ T) → (typeOfᴮ H Γ (B [ v / x ]ᴮunless no r) ≮: V) → Either (typeOfᴮ H Γ′ B ≮: V) (typeOfᴱ H ∅ (val v) ≮: T)

substitutivityᴱ H (var y) v x p = substitutivityᴱ-whenever H v x y (x ≡ⱽ y) p
substitutivityᴱ H (val w) v x p = Left p
substitutivityᴱ H (binexp M op N) v x p = Left p
substitutivityᴱ H (M $ N) v x p = mapL src-tgt-≮: (substitutivityᴱ H M v x (tgt-src-≮: p))
substitutivityᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x p = Left p
substitutivityᴱ H (block var b ∈ T is B end) v x p = Left p
substitutivityᴱ-whenever H v x x (yes refl) q = swapLR (≮:-antitrans q)
substitutivityᴱ-whenever H v x y (no p) q = Left (≡-trans-≮: (cong orAny (sym (⊕-lookup-miss x y _ _ p))) q)

substitutivityᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x p = substitutivityᴮ-unless H B v x f (x ≡ⱽ f) p
substitutivityᴮ H (local var y ∈ T ← M ∙ B) v x p = substitutivityᴮ-unless H B v x y (x ≡ⱽ y) p
substitutivityᴮ H (return M ∙ B) v x p = substitutivityᴱ H M v x p
substitutivityᴮ H done v x p = Left p
substitutivityᴮ-unless H B v x y (yes p) q = substitutivityᴮ-unless-yes H B v x y p (⊕-over p) q
substitutivityᴮ-unless H B v x y (no p) q = substitutivityᴮ-unless-no H B v x y p (⊕-swap p) q
substitutivityᴮ-unless-yes H B v x y refl refl p = Left p
substitutivityᴮ-unless-no H B v x y p refl q = substitutivityᴮ H B v x q

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
binOpPreservation H (·· v w) = refl

reflect-subtypingᴱ : ∀ H M {H′ M′ T} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → (typeOfᴱ H′ ∅ M′ ≮: T) → Either (typeOfᴱ H ∅ M ≮: T) (Warningᴱ H (typeCheckᴱ H ∅ M))
reflect-subtypingᴮ : ∀ H B {H′ B′ T} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → (typeOfᴮ H′ ∅ B′ ≮: T) → Either (typeOfᴮ H ∅ B ≮: T) (Warningᴮ H (typeCheckᴮ H ∅ B))

reflect-subtypingᴱ H (M $ N) (app₁ s) p = mapLR src-tgt-≮: app₁ (reflect-subtypingᴱ H M s (tgt-src-≮: p))
reflect-subtypingᴱ H (M $ N) (app₂ v s) p = Left (src-tgt-≮: (heap-weakeningᴱ ∅ H M (rednᴱ⊑ s) (tgt-src-≮: p)))
reflect-subtypingᴱ H (M $ N) (beta (function f ⟨ var y ∈ T ⟩∈ U is B end) v refl q) p = Left (≡-trans-≮: (cong tgt (cong orAny (cong typeOfᴹᴼ q))) p)
reflect-subtypingᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) p = Left p
reflect-subtypingᴱ H (block var b ∈ T is B end) (block s) p = Left p
reflect-subtypingᴱ H (block var b ∈ T is return (val v) ∙ B end) (return v) p = mapR BlockMismatch (swapLR (≮:-antitrans p))
reflect-subtypingᴱ H (block var b ∈ T is done end) done p = mapR BlockMismatch (swapLR (≮:-antitrans p))
reflect-subtypingᴱ H (binexp M op N) (binOp₀ s) p = Left (≡-trans-≮: (binOpPreservation H s) p)
reflect-subtypingᴱ H (binexp M op N) (binOp₁ s) p = Left p
reflect-subtypingᴱ H (binexp M op N) (binOp₂ s) p = Left p

reflect-subtypingᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) p = mapLR (heap-weakeningᴮ _ _ B (snoc defn)) (CONTRADICTION ∘ ≮:-antirefl) (substitutivityᴮ _ B (addr a) f p)
reflect-subtypingᴮ H (local var x ∈ T ← M ∙ B) (local s) p = Left (heap-weakeningᴮ (x ↦ T) H B (rednᴱ⊑ s) p)
reflect-subtypingᴮ H (local var x ∈ T ← M ∙ B) (subst v) p = mapR LocalVarMismatch (substitutivityᴮ H B v x p) 
reflect-subtypingᴮ H (return M ∙ B) (return s) p = mapR return (reflect-subtypingᴱ H M s p)

reflect-substitutionᴱ : ∀ {Γ T} H M v x → Warningᴱ H (typeCheckᴱ H Γ (M [ v / x ]ᴱ)) → Either (Warningᴱ H (typeCheckᴱ H (Γ ⊕ x ↦ T) M)) (Either (Warningᴱ H (typeCheckᴱ H ∅ (val v))) (typeOfᴱ H ∅ (val v) ≮: T))
reflect-substitutionᴱ-whenever : ∀ {Γ T} H v x y (p : Dec(x ≡ y)) → Warningᴱ H (typeCheckᴱ H Γ (var y [ v / x ]ᴱwhenever p)) → Either (Warningᴱ H (typeCheckᴱ H (Γ ⊕ x ↦ T) (var y))) (Either (Warningᴱ H (typeCheckᴱ H ∅ (val v))) (typeOfᴱ H ∅ (val v) ≮: T))
reflect-substitutionᴮ : ∀ {Γ T} H B v x → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮ)) → Either (Warningᴮ H (typeCheckᴮ H (Γ ⊕ x ↦ T) B)) (Either (Warningᴱ H (typeCheckᴱ H ∅ (val v))) (typeOfᴱ H ∅ (val v) ≮: T))
reflect-substitutionᴮ-unless : ∀ {Γ T U} H B v x y (r : Dec(x ≡ y)) → Warningᴮ H (typeCheckᴮ H (Γ ⊕ y ↦ U) (B [ v / x ]ᴮunless r)) → Either (Warningᴮ H (typeCheckᴮ H ((Γ ⊕ x ↦ T) ⊕ y ↦ U) B)) (Either (Warningᴱ H (typeCheckᴱ H ∅ (val v))) (typeOfᴱ H ∅ (val v) ≮: T))
reflect-substitutionᴮ-unless-yes : ∀ {Γ Γ′ T} H B v x y (r : x ≡ y) → (Γ′ ≡ Γ) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless yes r)) → Either (Warningᴮ H (typeCheckᴮ H Γ′ B)) (Either (Warningᴱ H (typeCheckᴱ H ∅ (val v))) (typeOfᴱ H ∅ (val v) ≮: T))
reflect-substitutionᴮ-unless-no : ∀ {Γ Γ′ T} H B v x y (r : x ≢ y) → (Γ′ ≡ Γ ⊕ x ↦ T) → Warningᴮ H (typeCheckᴮ H Γ (B [ v / x ]ᴮunless no r)) → Either (Warningᴮ H (typeCheckᴮ H Γ′ B)) (Either (Warningᴱ H (typeCheckᴱ H ∅ (val v))) (typeOfᴱ H ∅ (val v) ≮: T))

reflect-substitutionᴱ H (var y) v x W = reflect-substitutionᴱ-whenever H v x y (x ≡ⱽ y) W
reflect-substitutionᴱ H (val (addr a)) v x (UnallocatedAddress r) = Left (UnallocatedAddress r)
reflect-substitutionᴱ H (M $ N) v x (FunctionCallMismatch p) with substitutivityᴱ H N v x p
reflect-substitutionᴱ H (M $ N) v x (FunctionCallMismatch p) | Right W = Right (Right W)
reflect-substitutionᴱ H (M $ N) v x (FunctionCallMismatch p) | Left q with substitutivityᴱ H M v x (src-≮: q)
reflect-substitutionᴱ H (M $ N) v x (FunctionCallMismatch p) | Left q | Right W = Right (Right W)
reflect-substitutionᴱ H (M $ N) v x (FunctionCallMismatch p) | Left q | Left r = Left ((FunctionCallMismatch ∘ any-src-≮:) r)
reflect-substitutionᴱ H (M $ N) v x (app₁ W) = mapL app₁ (reflect-substitutionᴱ H M v x W)
reflect-substitutionᴱ H (M $ N) v x (app₂ W) = mapL app₂ (reflect-substitutionᴱ H N v x W)
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x (FunctionDefnMismatch q) = mapLR FunctionDefnMismatch Right (substitutivityᴮ-unless H B v x y (x ≡ⱽ y) q)
reflect-substitutionᴱ H (function f ⟨ var y ∈ T ⟩∈ U is B end) v x (function₁ W) = mapL function₁ (reflect-substitutionᴮ-unless H B v x y (x ≡ⱽ y) W)
reflect-substitutionᴱ H (block var b ∈ T is B end) v x (BlockMismatch q) =  mapLR BlockMismatch Right (substitutivityᴮ H B v x q)
reflect-substitutionᴱ H (block var b ∈ T is B end) v x (block₁ W′) = mapL block₁ (reflect-substitutionᴮ H B v x W′)
reflect-substitutionᴱ H (binexp M op N) v x (BinOpMismatch₁ q) = mapLR BinOpMismatch₁ Right (substitutivityᴱ H M v x q)
reflect-substitutionᴱ H (binexp M op N) v x (BinOpMismatch₂ q) = mapLR BinOpMismatch₂ Right (substitutivityᴱ H N v x q)
reflect-substitutionᴱ H (binexp M op N) v x (bin₁ W) = mapL bin₁ (reflect-substitutionᴱ H M v x W)
reflect-substitutionᴱ H (binexp M op N) v x (bin₂ W) = mapL bin₂ (reflect-substitutionᴱ H N v x W)

reflect-substitutionᴱ-whenever H a x x (yes refl) (UnallocatedAddress p) = Right (Left (UnallocatedAddress p))
reflect-substitutionᴱ-whenever H v x y (no p) (UnboundVariable q) = Left (UnboundVariable (trans (sym (⊕-lookup-miss x y _ _ p)) q))

reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x (FunctionDefnMismatch q) = mapLR FunctionDefnMismatch Right (substitutivityᴮ-unless H C v x y (x ≡ⱽ y) q)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x (function₁ W) =  mapL function₁ (reflect-substitutionᴮ-unless H C v x y (x ≡ⱽ y) W)
reflect-substitutionᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) v x (function₂ W) = mapL function₂ (reflect-substitutionᴮ-unless H B v x f (x ≡ⱽ f) W)
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x (LocalVarMismatch q) = mapLR LocalVarMismatch Right (substitutivityᴱ H M v x q)
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x (local₁ W) = mapL local₁ (reflect-substitutionᴱ H M v x W)
reflect-substitutionᴮ H (local var y ∈ T ← M ∙ B) v x (local₂ W) = mapL local₂ (reflect-substitutionᴮ-unless H B v x y (x ≡ⱽ y) W)
reflect-substitutionᴮ H (return M ∙ B) v x (return W) = mapL return (reflect-substitutionᴱ H M v x W)

reflect-substitutionᴮ-unless H B v x y (yes p) W = reflect-substitutionᴮ-unless-yes H B v x y p (⊕-over p) W
reflect-substitutionᴮ-unless H B v x y (no p) W = reflect-substitutionᴮ-unless-no H B v x y p (⊕-swap p) W
reflect-substitutionᴮ-unless-yes H B v x x refl refl W = Left W
reflect-substitutionᴮ-unless-no H B v x y p refl W = reflect-substitutionᴮ H B v x W

reflect-weakeningᴱ : ∀ Γ H M {H′} → (H ⊑ H′) → Warningᴱ H′ (typeCheckᴱ H′ Γ M) → Warningᴱ H (typeCheckᴱ H Γ M)
reflect-weakeningᴮ : ∀ Γ H B {H′} → (H ⊑ H′) → Warningᴮ H′ (typeCheckᴮ H′ Γ B) → Warningᴮ H (typeCheckᴮ H Γ B)

reflect-weakeningᴱ Γ H (var x) h (UnboundVariable p) = (UnboundVariable p)
reflect-weakeningᴱ Γ H (val (addr a)) h (UnallocatedAddress p) = UnallocatedAddress (lookup-⊑-nothing a h p)
reflect-weakeningᴱ Γ H (M $ N) h (FunctionCallMismatch p) = FunctionCallMismatch (heap-weakeningᴱ Γ H N h (any-src-≮: (heap-weakeningᴱ Γ H M h (src-≮: p))))
reflect-weakeningᴱ Γ H (M $ N) h (app₁ W) = app₁ (reflect-weakeningᴱ Γ H M h W)
reflect-weakeningᴱ Γ H (M $ N) h (app₂ W) = app₂ (reflect-weakeningᴱ Γ H N h W)
reflect-weakeningᴱ Γ H (binexp M op N) h (BinOpMismatch₁ p) = BinOpMismatch₁ (heap-weakeningᴱ Γ H M h p)
reflect-weakeningᴱ Γ H (binexp M op N) h (BinOpMismatch₂ p) = BinOpMismatch₂ (heap-weakeningᴱ Γ H N h p)
reflect-weakeningᴱ Γ H (binexp M op N) h (bin₁ W′) = bin₁ (reflect-weakeningᴱ Γ H M h W′)
reflect-weakeningᴱ Γ H (binexp M op N) h (bin₂ W′) = bin₂ (reflect-weakeningᴱ Γ H N h W′)
reflect-weakeningᴱ Γ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (FunctionDefnMismatch p) = FunctionDefnMismatch (heap-weakeningᴮ (Γ ⊕ y ↦ T) H B h p)
reflect-weakeningᴱ Γ H (function f ⟨ var y ∈ T ⟩∈ U is B end) h (function₁ W) = function₁ (reflect-weakeningᴮ (Γ ⊕ y ↦ T) H B h W)
reflect-weakeningᴱ Γ H (block var b ∈ T is B end) h (BlockMismatch p) = BlockMismatch (heap-weakeningᴮ Γ H B h p)
reflect-weakeningᴱ Γ H (block var b ∈ T is B end) h (block₁ W) = block₁ (reflect-weakeningᴮ Γ H B h W)

reflect-weakeningᴮ Γ H (return M ∙ B) h (return W) = return (reflect-weakeningᴱ Γ H M h W)
reflect-weakeningᴮ Γ H (local var y ∈ T ← M ∙ B) h (LocalVarMismatch p) = LocalVarMismatch (heap-weakeningᴱ Γ H M h p)
reflect-weakeningᴮ Γ H (local var y ∈ T ← M ∙ B) h (local₁ W) = local₁ (reflect-weakeningᴱ Γ H M h W)
reflect-weakeningᴮ Γ H (local var y ∈ T ← M ∙ B) h (local₂ W) = local₂ (reflect-weakeningᴮ (Γ ⊕ y ↦ T) H B h W)
reflect-weakeningᴮ Γ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (FunctionDefnMismatch p) = FunctionDefnMismatch (heap-weakeningᴮ (Γ ⊕ x ↦ T) H C h p)
reflect-weakeningᴮ Γ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₁ W) = function₁ (reflect-weakeningᴮ (Γ ⊕ x ↦ T) H C h W)
reflect-weakeningᴮ Γ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) h (function₂ W) = function₂ (reflect-weakeningᴮ (Γ ⊕ f ↦ (T ⇒ U)) H B h W)

reflect-weakeningᴼ : ∀ H O {H′} → (H ⊑ H′) → Warningᴼ H′ (typeCheckᴼ H′ O) → Warningᴼ H (typeCheckᴼ H O)
reflect-weakeningᴼ H (just function f ⟨ var x ∈ T ⟩∈ U is B end) h (FunctionDefnMismatch p) = FunctionDefnMismatch (heap-weakeningᴮ (x ↦ T) H B h p)
reflect-weakeningᴼ H (just function f ⟨ var x ∈ T ⟩∈ U is B end) h (function₁ W) = function₁ (reflect-weakeningᴮ (x ↦ T) H B h W)

reflectᴱ : ∀ H M {H′ M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴱ H′ (typeCheckᴱ H′ ∅ M′) → Either (Warningᴱ H (typeCheckᴱ H ∅ M)) (Warningᴴ H (typeCheckᴴ H))
reflectᴮ : ∀ H B {H′ B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴮ H′ (typeCheckᴮ H′ ∅ B′) → Either (Warningᴮ H (typeCheckᴮ H ∅ B)) (Warningᴴ H (typeCheckᴴ H))

reflectᴱ H (M $ N) (app₁ s) (FunctionCallMismatch p) = cond (Left ∘ FunctionCallMismatch ∘ heap-weakeningᴱ ∅ H N (rednᴱ⊑ s) ∘ any-src-≮:) (Left ∘ app₁) (reflect-subtypingᴱ H M s (src-≮: p))
reflectᴱ H (M $ N) (app₁ s) (app₁ W′) = mapL app₁ (reflectᴱ H M s W′)
reflectᴱ H (M $ N) (app₁ s) (app₂ W′) = Left (app₂ (reflect-weakeningᴱ ∅ H N (rednᴱ⊑ s) W′))
reflectᴱ H (M $ N) (app₂ p s) (FunctionCallMismatch q) = cond (Left ∘ FunctionCallMismatch ∘ any-src-≮: ∘ heap-weakeningᴱ ∅ H M (rednᴱ⊑ s) ∘ src-≮:) (Left ∘ app₂) (reflect-subtypingᴱ H N s q)
reflectᴱ H (M $ N) (app₂ p s) (app₁ W′) = Left (app₁ (reflect-weakeningᴱ ∅ H M (rednᴱ⊑ s) W′))
reflectᴱ H (M $ N) (app₂ p s) (app₂ W′) = mapL app₂ (reflectᴱ H N s W′)
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) with substitutivityᴮ H B v x q 
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | Left r = Right (addr a p (FunctionDefnMismatch r))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (BlockMismatch q) | Right r = Left (FunctionCallMismatch (≮:-trans-≡ r ((cong src (cong orAny (cong typeOfᴹᴼ (sym p)))))))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) with reflect-substitutionᴮ _ B v x W′
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | Left W = Right (addr a p (function₁ W))
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | Right (Left W) = Left (app₂ W)
reflectᴱ H (val (addr a) $ N) (beta (function f ⟨ var x ∈ T ⟩∈ U is B end) v refl p) (block₁ W′) | Right (Right q) = Left (FunctionCallMismatch (≮:-trans-≡ q (cong src (cong orAny (cong typeOfᴹᴼ (sym p))))))
reflectᴱ H (block var b ∈ T is B end) (block s) (BlockMismatch p) = Left (cond BlockMismatch block₁ (reflect-subtypingᴮ H B s p))
reflectᴱ H (block var b ∈ T is B end) (block s) (block₁ W′) = mapL block₁ (reflectᴮ H B s W′)
reflectᴱ H (block var b ∈ T is B end) (return v) W′ = Left (block₁ (return W′))
reflectᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (UnallocatedAddress ())
reflectᴱ H (binexp M op N) (binOp₀ ()) (UnallocatedAddress p)
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₁ p) = Left (cond BinOpMismatch₁ bin₁ (reflect-subtypingᴱ H M s p))
reflectᴱ H (binexp M op N) (binOp₁ s) (BinOpMismatch₂ p) = Left (BinOpMismatch₂ (heap-weakeningᴱ ∅ H N (rednᴱ⊑ s) p))
reflectᴱ H (binexp M op N) (binOp₁ s) (bin₁ W′) = mapL bin₁ (reflectᴱ H M s W′)
reflectᴱ H (binexp M op N) (binOp₁ s) (bin₂ W′) = Left (bin₂ (reflect-weakeningᴱ ∅ H N (rednᴱ⊑ s) W′))
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₁ p) = Left (BinOpMismatch₁ (heap-weakeningᴱ ∅ H M (rednᴱ⊑ s) p))
reflectᴱ H (binexp M op N) (binOp₂ s) (BinOpMismatch₂ p) = Left (cond BinOpMismatch₂ bin₂ (reflect-subtypingᴱ H N s p))
reflectᴱ H (binexp M op N) (binOp₂ s) (bin₁ W′) = Left (bin₁ (reflect-weakeningᴱ ∅ H M (rednᴱ⊑ s) W′))
reflectᴱ H (binexp M op N) (binOp₂ s) (bin₂ W′) = mapL bin₂ (reflectᴱ H N s W′)

reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (LocalVarMismatch p) = Left (cond LocalVarMismatch local₁ (reflect-subtypingᴱ H M s p))
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₁ W′) = mapL local₁ (reflectᴱ H M s W′)
reflectᴮ H (local var x ∈ T ← M ∙ B) (local s) (local₂ W′) = Left (local₂ (reflect-weakeningᴮ (x ↦ T) H B (rednᴱ⊑ s) W′))
reflectᴮ H (local var x ∈ T ← M ∙ B) (subst v) W′ = Left (cond local₂ (cond local₁ LocalVarMismatch) (reflect-substitutionᴮ H B v x W′))
reflectᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) W′ with reflect-substitutionᴮ _ B (addr a) f W′
reflectᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) W′ | Left W = Left (function₂ (reflect-weakeningᴮ (f ↦ (T ⇒ U)) H B (snoc defn) W))
reflectᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) W′ | Right (Left (UnallocatedAddress ()))
reflectᴮ H (function f ⟨ var y ∈ T ⟩∈ U is C end ∙ B) (function a defn) W′ | Right (Right p) = CONTRADICTION (≮:-antirefl p)
reflectᴮ H (return M ∙ B) (return s) (return W′) = mapL return (reflectᴱ H M s W′)

reflectᴴᴱ : ∀ H M {H′ M′} → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → Warningᴴ H′ (typeCheckᴴ H′) → Either (Warningᴱ H (typeCheckᴱ H ∅ M)) (Warningᴴ H (typeCheckᴴ H))
reflectᴴᴮ : ∀ H B {H′ B′} → (H ⊢ B ⟶ᴮ B′ ⊣ H′) → Warningᴴ H′ (typeCheckᴴ H′) → Either (Warningᴮ H (typeCheckᴮ H ∅ B)) (Warningᴴ H (typeCheckᴴ H))

reflectᴴᴱ H (M $ N) (app₁ s) W = mapL app₁ (reflectᴴᴱ H M s W)
reflectᴴᴱ H (M $ N) (app₂ v s) W = mapL app₂ (reflectᴴᴱ H N s W)
reflectᴴᴱ H (M $ N) (beta O v refl p) W = Right W
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a p) (addr b refl W) with b ≡ᴬ a
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (addr b refl (FunctionDefnMismatch p)) | yes refl = Left (FunctionDefnMismatch (heap-weakeningᴮ (x ↦ T) H B (snoc defn) p))
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a defn) (addr b refl (function₁ W)) | yes refl = Left (function₁ (reflect-weakeningᴮ (x ↦ T) H B (snoc defn) W))
reflectᴴᴱ H (function f ⟨ var x ∈ T ⟩∈ U is B end) (function a p) (addr b refl W) | no q = Right (addr b (lookup-not-allocated p q) (reflect-weakeningᴼ H _ (snoc p) W))
reflectᴴᴱ H (block var b ∈ T is B end) (block s) W = mapL block₁ (reflectᴴᴮ H B s W)
reflectᴴᴱ H (block var b ∈ T is return (val v) ∙ B end) (return v) W = Right W
reflectᴴᴱ H (block var b ∈ T is done end) done W = Right W
reflectᴴᴱ H (binexp M op N) (binOp₀ s) W = Right W
reflectᴴᴱ H (binexp M op N) (binOp₁ s) W = mapL bin₁ (reflectᴴᴱ H M s W)
reflectᴴᴱ H (binexp M op N) (binOp₂ s) W = mapL bin₂ (reflectᴴᴱ H N s W)

reflectᴴᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a p) (addr b refl W) with b ≡ᴬ a
reflectᴴᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) (addr b refl (FunctionDefnMismatch p)) | yes refl = Left (FunctionDefnMismatch (heap-weakeningᴮ (x ↦ T) H C (snoc defn) p))
reflectᴴᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a defn) (addr b refl (function₁ W)) | yes refl = Left (function₁ (reflect-weakeningᴮ (x ↦ T) H C (snoc defn) W))
reflectᴴᴮ H (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) (function a p) (addr b refl W) | no q = Right (addr b (lookup-not-allocated p q) (reflect-weakeningᴼ H _ (snoc p) W))
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (local s) W = mapL local₁ (reflectᴴᴱ H M s W)
reflectᴴᴮ H (local var x ∈ T ← M ∙ B) (subst v) W = Right W
reflectᴴᴮ H (return M ∙ B) (return s) W = mapL return (reflectᴴᴱ H M s W)

reflect* : ∀ H B {H′ B′} → (H ⊢ B ⟶* B′ ⊣ H′) → Either (Warningᴮ H′ (typeCheckᴮ H′ ∅ B′)) (Warningᴴ H′ (typeCheckᴴ H′)) → Either (Warningᴮ H (typeCheckᴮ H ∅ B)) (Warningᴴ H (typeCheckᴴ H))
reflect* H B refl W = W
reflect* H B (step s t) W = cond (reflectᴮ H B s) (reflectᴴᴮ H B s) (reflect* _ _ t W)

isntNumber : ∀ H v → (valueType v ≢ number) → (typeOfᴱ H ∅ (val v) ≮: number)
isntNumber H nil p = scalar-≢-impl-≮: nil number (λ ())
isntNumber H (addr a) p with remember (H [ a ]ᴴ)
isntNumber H (addr a) p | (just (function f ⟨ var x ∈ T ⟩∈ U is B end) , q) = ≡-trans-≮: (cong orAny (cong typeOfᴹᴼ q)) (function-≮:-scalar number)
isntNumber H (addr a) p | (nothing , q) = ≡-trans-≮: (cong orAny (cong typeOfᴹᴼ q)) (any-≮:-scalar number)
isntNumber H (number x) p = CONTRADICTION (p refl)
isntNumber H (bool x) p = scalar-≢-impl-≮: boolean number (λ ())
isntNumber H (string x) p = scalar-≢-impl-≮: string number (λ ())

isntString : ∀ H v → (valueType v ≢ string) → (typeOfᴱ H ∅ (val v) ≮: string)
isntString H nil p = scalar-≢-impl-≮: nil string (λ ())
isntString H (addr a) p with remember (H [ a ]ᴴ)
isntString H (addr a) p | (just (function f ⟨ var x ∈ T ⟩∈ U is B end) , q) = ≡-trans-≮: (cong orAny (cong typeOfᴹᴼ q)) (function-≮:-scalar string)
isntString H (addr a) p | (nothing , q) = ≡-trans-≮: (cong orAny (cong typeOfᴹᴼ q)) (any-≮:-scalar string)
isntString H (number x) p = scalar-≢-impl-≮: number string (λ ())
isntString H (bool x) p = scalar-≢-impl-≮: boolean string (λ ())
isntString H (string x) p = CONTRADICTION (p refl)

isntFunction : ∀ H v {T U} → (valueType v ≢ function) → (typeOfᴱ H ∅ (val v) ≮: (T ⇒ U))
isntFunction H nil p = scalar-≮:-function nil
isntFunction H (addr a) p = CONTRADICTION (p refl)
isntFunction H (number x) p = scalar-≮:-function number
isntFunction H (bool x) p = scalar-≮:-function boolean
isntFunction H (string x) p = scalar-≮:-function string

runtimeBinOpWarning : ∀ H {op} v → BinOpError op (valueType v) → (typeOfᴱ H ∅ (val v) ≮: srcBinOp op)
runtimeBinOpWarning H v (+ p) = isntNumber H v p
runtimeBinOpWarning H v (- p) = isntNumber H v p
runtimeBinOpWarning H v (* p) = isntNumber H v p
runtimeBinOpWarning H v (/ p) = isntNumber H v p
runtimeBinOpWarning H v (< p) = isntNumber H v p
runtimeBinOpWarning H v (> p) = isntNumber H v p
runtimeBinOpWarning H v (<= p) = isntNumber H v p
runtimeBinOpWarning H v (>= p) = isntNumber H v p
runtimeBinOpWarning H v (·· p) = isntString H v p

runtimeWarningᴱ : ∀ H M → RuntimeErrorᴱ H M → Warningᴱ H (typeCheckᴱ H ∅ M)
runtimeWarningᴮ : ∀ H B → RuntimeErrorᴮ H B → Warningᴮ H (typeCheckᴮ H ∅ B)

runtimeWarningᴱ H (var x) UnboundVariable = UnboundVariable refl
runtimeWarningᴱ H (val (addr a)) (SEGV p) = UnallocatedAddress p
runtimeWarningᴱ H (M $ N) (FunctionMismatch v w p) = FunctionCallMismatch (any-src-≮: (isntFunction H v p))
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
wellTypedProgramsDontGoWrong H′ B B′ t err with reflect* ∅ᴴ B t (Left (runtimeWarningᴮ H′ B′ err))
wellTypedProgramsDontGoWrong H′ B B′ t err | Right (addr a refl ())
wellTypedProgramsDontGoWrong H′ B B′ t err | Left W = W
