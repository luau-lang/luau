module Properties.TypeCheck where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either)
open import Luau.TypeCheck using (_▷_⊢ᴱ_∋_∈_⊣_; _▷_⊢ᴮ_∋_∈_⊣_; nil; var; addr; app)
open import Luau.Syntax using (Block; Expr; yes; nil; var; addr; _$_; function_is_end; block_is_end; _∙_; return; done; local_←_; _⟨_⟩; _⟨_⟩∈_; var_∈_; name; fun; arg)
open import Luau.Type using (Type; nil; none; _⇒_; src; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _↦_; _⊕_↦_; _⋒_; ⊕-[]) renaming (_[_] to _[_]ⱽ)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var; _≡ⱽ_)
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Properties.Dec using (⊥; yes; no)
open import Properties.Remember using (remember; _,_)

sym : ∀ {A : Set} {a b : A} → (a ≡ b) → (b ≡ a)
sym refl = refl

trans : ∀ {A : Set} {a b c : A} → (a ≡ b) → (b ≡ c) → (a ≡ c)
trans refl refl = refl

cong : ∀ {A B : Set} {a b : A} (f : A → B) → (a ≡ b) → (f a ≡ f b)
cong f refl = refl

_⊆_ : ∀ {A : Set} → (A → Set) → (A → Set) → Set
P ⊆ Q = (∀ a → P a → Q a)

data _⊝_ {A : Set} (P : A → Set) (a b : A) : Set where
  _,_ : (P b) → ((a ≡ b) → ⊥) → (P ⊝ a) b

data _∪_ {A : Set} (P Q : A → Set) (a : A) : Set where
  left : (P a) → (P ∪ Q) a
  right : (Q a) → (P ∪ Q) a

∪-⊆ : ∀ {A : Set} {P Q R : A → Set} → (P ⊆ R) → (Q ⊆ R) → ((P ∪ Q) ⊆ R)
∪-⊆ p q a (left r) = p a r
∪-⊆ p q a (right r) = q a r

⊆-left : {A : Set} {P Q R : A → Set} → ((P ∪ Q) ⊆ R) → (P ⊆ R)
⊆-left p a q = p a (left q)

⊆-right : {A : Set} {P Q R : A → Set} → ((P ∪ Q) ⊆ R) → (Q ⊆ R)
⊆-right p a q = p a (right q)

sing : ∀ {A} → A → A → Set
sing = _≡_

data emp {A : Set} : A → Set where

fvᴱ : ∀ {a} → Expr a → Var → Set
fvᴮ : ∀ {a} → Block a → Var → Set

fvᴱ nil = emp
fvᴱ (var x) = sing x
fvᴱ (addr x) = emp
fvᴱ (M $ N) = fvᴱ M ∪ fvᴱ N
fvᴱ function F is B end = fvᴮ B ⊝ name (arg F)
fvᴱ block b is B end = fvᴮ B

fvᴮ (function F is C end ∙ B) = (fvᴮ C ⊝ name (arg F)) ∪ (fvᴮ B ⊝ fun F)
fvᴮ (local x ← M ∙ B) = fvᴱ M ∪ (fvᴮ B ⊝ name x)
fvᴮ (return M ∙ B) = fvᴱ M ∪ fvᴮ B
fvᴮ done = emp

faᴱ : ∀ {a} → Expr a → Addr → Set
faᴮ : ∀ {a} → Block a → Addr → Set

faᴱ nil = emp
faᴱ (var x) = emp
faᴱ (addr a) = sing a
faᴱ (M $ N) = faᴱ M ∪ faᴱ N
faᴱ function F is B end = faᴮ B
faᴱ block b is B end = faᴮ B

faᴮ (function F is C end ∙ B) = faᴮ C ∪ faᴮ B
faᴮ (local x ← M ∙ B) =  faᴱ M ∪ faᴮ B
faᴮ (return M ∙ B) = faᴱ M ∪ faᴮ B
faᴮ done = emp

data dv (Γ : VarCtxt) (x : Var) : Set where
  just : ∀ T → (just T ≡ Γ [ x ]ⱽ) → dv Γ x

data da (Σ : AddrCtxt) (a : Addr) : Set where
  just : ∀ T → (just T ≡ Σ [ a ]ᴬ) → da Σ a

⊕-⊆-⊝ : ∀ {Γ P} x T → ((P ⊝ x) ⊆ dv Γ) → (P ⊆ dv (Γ ⊕ x ↦ T))
⊕-⊆-⊝ x T p y q with x ≡ⱽ y
⊕-⊆-⊝ x T p .x q | yes refl = just T (sym (⊕-[] _ x T))
⊕-⊆-⊝ x T p y q | no r = {!!}

orNone : Maybe Type → Type
orNone nothing = none
orNone (just T) = T

dv-orNone : ∀ {Γ x} → (dv Γ x) → (just (orNone (Γ [ x ]ⱽ)) ≡ Γ [ x ]ⱽ)
dv-orNone (just T p) = trans (sym (cong just (cong orNone p))) p

da-orNone : ∀ {Σ a} → (da Σ a) → (just (orNone (Σ [ a ]ᴬ)) ≡ Σ [ a ]ᴬ)
da-orNone (just T p) = trans (sym (cong just (cong orNone p))) p


typeOfᴱ : AddrCtxt → VarCtxt → (Expr yes) → Type
typeOfᴮ : AddrCtxt → VarCtxt → (Block yes) → Type

typeOfᴱ Σ Γ nil = nil
typeOfᴱ Σ Γ (var x) = orNone(Γ [ x ]ⱽ)
typeOfᴱ Σ Γ (addr a) = orNone(Σ [ a ]ᴬ)
typeOfᴱ Σ Γ (M $ N) = tgt(typeOfᴱ Σ Γ M)
typeOfᴱ Σ Γ (function f ⟨ var x ∈ S ⟩∈ T is B end) = S ⇒ T
typeOfᴱ Σ Γ (block b is B end) = typeOfᴮ Σ Γ B

typeOfᴮ Σ Γ (function f ⟨ var x ∈ S ⟩∈ T is C end ∙ B) = typeOfᴮ Σ (Γ ⊕ f ↦ (S ⇒ T)) B
typeOfᴮ Σ Γ (local var x ∈ T ← M ∙ B) = typeOfᴮ Σ (Γ ⊕ x ↦ T) B
typeOfᴮ Σ Γ (return M ∙ B) = typeOfᴱ Σ Γ M
typeOfᴮ Σ Γ done = nil

data TypeCheckResultᴱ (Σ : AddrCtxt) (Γ : VarCtxt) (S : Type) (M : Expr yes) : Set
data TypeCheckResultᴮ (Σ : AddrCtxt) (Γ : VarCtxt) (S : Type) (B : Block yes) : Set

data TypeCheckResultᴱ Σ Γ S M where

  ok : ∀ Δ → (Σ ▷ Γ ⊢ᴱ S ∋ M ∈ (typeOfᴱ Σ Γ M) ⊣ Δ) → TypeCheckResultᴱ Σ Γ S M
  
data TypeCheckResultᴮ Σ Γ S B where

  ok : ∀ Δ → (Σ ▷ Γ ⊢ᴮ S ∋ B ∈ (typeOfᴮ Σ Γ B) ⊣ Δ) → TypeCheckResultᴮ Σ Γ S B
  
typeCheckᴱ : ∀ Σ Γ S M → (faᴱ M ⊆ da Σ) → (fvᴱ M ⊆ dv Γ) → (TypeCheckResultᴱ Σ Γ S M)
typeCheckᴮ : ∀ Σ Γ S B → (faᴮ B ⊆ da Σ) → (fvᴮ B ⊆ dv Γ) → (TypeCheckResultᴮ Σ Γ S B)

typeCheckᴱ Σ Γ S nil p q = ok ∅ nil
typeCheckᴱ Σ Γ S (var x) p q = ok (x ↦ S) (var x (dv-orNone (q x refl)))
typeCheckᴱ Σ Γ S (addr a) p q = ok ∅ (addr a (da-orNone (p a refl)))
typeCheckᴱ Σ Γ S (M $ N) p q with typeCheckᴱ Σ Γ (typeOfᴱ Σ Γ N ⇒ S) M (⊆-left p) (⊆-left q) | typeCheckᴱ Σ Γ (src (typeOfᴱ Σ Γ M)) N (⊆-right p) (⊆-right q)
typeCheckᴱ Σ Γ S (M $ N) p q | ok Δ₁ r | ok Δ₂ s = ok (Δ₁ ⋒ Δ₂) (app r s)
typeCheckᴱ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) p q with typeCheckᴮ Σ (Γ ⊕ x ↦ T) U B p {!q!}
typeCheckᴱ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) p q | R = {!!}
typeCheckᴱ Σ Γ S (block b is B end) p q = {!!}

typeCheckᴮ Σ Γ S B = {!!}
