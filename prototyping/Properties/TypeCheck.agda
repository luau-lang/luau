open import Luau.Type using (Mode)

module Properties.TypeCheck (m : Mode) where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either)
open import Luau.TypeCheck(m) using (_▷_⊢ᴱ_∋_∈_⊣_; _▷_⊢ᴮ_∋_∈_⊣_; nil; var; addr; app; function; block; done; return; local)
open import Luau.Syntax using (Block; Expr; yes; nil; var; addr; _$_; function_is_end; block_is_end; _∙_; return; done; local_←_; _⟨_⟩; _⟨_⟩∈_; var_∈_; name; fun; arg)
open import Luau.Type using (Type; nil; top; _⇒_; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _↦_; _⊕_↦_; _⋒_; _⊝_; ⊕-[]) renaming (_[_] to _[_]ⱽ)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var; _≡ⱽ_)
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Properties.Dec using (yes; no)
open import Properties.Equality using (_≢_; sym; trans; cong)
open import Properties.Remember using (remember; _,_)

src : Type → Type
src = Luau.Type.src m

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

contextOfᴱ : AddrCtxt → VarCtxt → Type → (Expr yes) → VarCtxt
contextOfᴮ : AddrCtxt → VarCtxt → Type → (Block yes) → VarCtxt

contextOfᴱ Σ Γ S nil = ∅
contextOfᴱ Σ Γ S (var x) = (x ↦ S)
contextOfᴱ Σ Γ S (addr a) = ∅
contextOfᴱ Σ Γ S (M $ N) = (contextOfᴱ Σ Γ (U ⇒ S) M) ⋒ (contextOfᴱ Σ Γ (src T) N) where T = typeOfᴱ Σ Γ M; U = typeOfᴱ Σ Γ N
contextOfᴱ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) = (contextOfᴮ Σ (Γ ⊕ x ↦ T) U B) ⊝ x
contextOfᴱ Σ Γ S (block b is B end) = (contextOfᴮ Σ Γ S B)

contextOfᴮ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) = ((contextOfᴮ Σ (Γ ⊕ x ↦ T) U C) ⊝ x) ⋒ ((contextOfᴮ Σ (Γ ⊕ f ↦ (T ⇒ U)) S B) ⊝ f)
contextOfᴮ Σ Γ S (local var x ∈ T ← M ∙ B) = (contextOfᴱ Σ Γ T M) ⋒ ((contextOfᴮ Σ (Γ ⊕ x ↦ T)S B) ⊝ x)
contextOfᴮ Σ Γ S (return M ∙ B) = (contextOfᴱ Σ Γ S M)
contextOfᴮ Σ Γ S done = ∅

typeCheckᴱ : ∀ Σ Γ S M → (Σ ▷ Γ ⊢ᴱ S ∋ M ∈ (typeOfᴱ Σ Γ M) ⊣ (contextOfᴱ Σ Γ S M))
typeCheckᴮ : ∀ Σ Γ S B → (Σ ▷ Γ ⊢ᴮ S ∋ B ∈ (typeOfᴮ Σ Γ B) ⊣ (contextOfᴮ Σ Γ S B))

typeCheckᴱ Σ Γ S nil = nil
typeCheckᴱ Σ Γ S (var x) = var x refl
typeCheckᴱ Σ Γ S (addr a) = addr a refl
typeCheckᴱ Σ Γ S (M $ N) = app (typeCheckᴱ Σ Γ (typeOfᴱ Σ Γ N ⇒ S) M) (typeCheckᴱ Σ Γ (src (typeOfᴱ Σ Γ M)) N)
typeCheckᴱ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) = function(typeCheckᴮ Σ (Γ ⊕ x ↦ T) U B)
typeCheckᴱ Σ Γ S (block b is B end) = block b (typeCheckᴮ Σ Γ S B)

typeCheckᴮ Σ Γ S (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) = function(typeCheckᴮ Σ (Γ ⊕ x ↦ T) U C) (typeCheckᴮ Σ (Γ ⊕ f ↦ (T ⇒ U)) S B)
typeCheckᴮ Σ Γ S (local var x ∈ T ← M ∙ B) = local (typeCheckᴱ Σ Γ T M) (typeCheckᴮ Σ (Γ ⊕ x ↦ T) S B)
typeCheckᴮ Σ Γ S (return M ∙ B) = return (typeCheckᴱ Σ Γ S M) (typeCheckᴮ Σ Γ top B)
typeCheckᴮ Σ Γ S done = done
