module Properties.TypeCheck where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either)
open import Luau.TypeCheck using (_▷_⊢ᴱ_∋_∈_⊣_; _▷_⊢ᴮ_∋_∈_⊣_; nil; var; addr; app; function; block; done; return; local)
open import Luau.Syntax using (Block; Expr; yes; nil; var; addr; _$_; function_is_end; block_is_end; _∙_; return; done; local_←_; _⟨_⟩; _⟨_⟩∈_; var_∈_; name; fun; arg)
open import Luau.Type using (Type; nil; none; _⇒_; src; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _↦_; _⊕_↦_; _⋒_; _⊝_; ⊕-[]) renaming (_[_] to _[_]ⱽ)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var; _≡ⱽ_)
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Properties.Dec using (yes; no)
open import Properties.Equality using (_≢_; sym; trans; cong)
open import Properties.Remember using (remember; _,_)

typeOfᴱ : AddrCtxt → VarCtxt → (Expr yes) → Type
typeOfᴮ : AddrCtxt → VarCtxt → (Block yes) → Type

typeOfᴱ Σ Γ nil = nil
typeOfᴱ Σ Γ (var x) = Γ [ x ]ⱽ
typeOfᴱ Σ Γ (addr a) = Σ [ a ]ᴬ
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
  
typeCheckᴱ : ∀ Σ Γ S M → (TypeCheckResultᴱ Σ Γ S M)
typeCheckᴮ : ∀ Σ Γ S B → (TypeCheckResultᴮ Σ Γ S B)

typeCheckᴱ Σ Γ S nil = ok ∅ nil
typeCheckᴱ Σ Γ S (var x) = ok (x ↦ S) (var x refl)
typeCheckᴱ Σ Γ S (addr a) = ok ∅ (addr a refl)
typeCheckᴱ Σ Γ S (M $ N) with typeCheckᴱ Σ Γ (typeOfᴱ Σ Γ N ⇒ S) M | typeCheckᴱ Σ Γ (src (typeOfᴱ Σ Γ M)) N
typeCheckᴱ Σ Γ S (M $ N) | ok Δ₁ D₁ | ok Δ₂ D₂ = ok (Δ₁ ⋒ Δ₂) (app D₁ D₂)
typeCheckᴱ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) with typeCheckᴮ Σ (Γ ⊕ x ↦ T) U B
typeCheckᴱ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) | ok Δ D = ok (Δ ⊝ x) (function D)
typeCheckᴱ Σ Γ S (block b is B end) with typeCheckᴮ Σ Γ S B
typeCheckᴱ Σ Γ S block b is B end | ok Δ D = ok Δ (block D)

typeCheckᴮ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) with typeCheckᴮ Σ (Γ ⊕ x ↦ T) U C | typeCheckᴮ Σ (Γ ⊕ f ↦ (T ⇒ U)) S B
typeCheckᴮ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) | ok Δ₁ D₁ | ok Δ₂ D₂ = ok ((Δ₁ ⊝ x) ⋒ (Δ₂ ⊝ f)) (function D₁ D₂)
typeCheckᴮ Σ Γ S (local var x ∈ T ← M ∙ B) with typeCheckᴱ Σ Γ T M | typeCheckᴮ Σ (Γ ⊕ x ↦ T) S B
typeCheckᴮ Σ Γ S (local var x ∈ T ← M ∙ B) | ok Δ₁ D₁ | ok Δ₂ D₂ = ok (Δ₁ ⋒ (Δ₂ ⊝ x)) (local D₁ D₂)
typeCheckᴮ Σ Γ S (return M ∙ B) with typeCheckᴱ Σ Γ S M
typeCheckᴮ Σ Γ S (return M ∙ B) | ok Δ D = ok Δ (return D)
typeCheckᴮ Σ Γ S done = ok ∅ done
