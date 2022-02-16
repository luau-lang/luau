open import Luau.Type using (Mode)

module Properties.TypeCheck (m : Mode) where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either)
open import Luau.TypeCheck(m) using (_⊢ᴱ_∋_∈_⊣_; _⊢ᴮ_∋_∈_⊣_; nil; var; addr; app; function; block; done; return; local)
open import Luau.Syntax using (Block; Expr; yes; nil; var; addr; _$_; function_is_end; block_is_end; _∙_; return; done; local_←_; _⟨_⟩; _⟨_⟩∈_; var_∈_; name; fun; arg)
open import Luau.Type using (Type; nil; top; bot; _⇒_; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _↦_; _⊕_↦_; _⋒_; _⊝_; ⊕-[]) renaming (_[_] to _[_]ⱽ)
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var; _≡ⱽ_)
open import Luau.Heap using (Heap; HeapValue; function_is_end) renaming (_[_] to _[_]ᴴ)
open import Properties.Dec using (yes; no)
open import Properties.Equality using (_≢_; sym; trans; cong)
open import Properties.Remember using (remember; _,_)

src : Type → Type
src = Luau.Type.src m

typeOfᴴ : Maybe(HeapValue yes) → Type
typeOfᴴ nothing = bot
typeOfᴴ (just function f ⟨ var x ∈ S ⟩∈ T is B end) = (S ⇒ T)

typeOfᴱ : Heap yes → VarCtxt → (Expr yes) → Type
typeOfᴮ : Heap yes → VarCtxt → (Block yes) → Type

typeOfᴱ H Γ nil = nil
typeOfᴱ H Γ (var x) = Γ [ x ]ⱽ
typeOfᴱ H Γ (addr a) = typeOfᴴ (H [ a ]ᴴ)
typeOfᴱ H Γ (M $ N) = tgt(typeOfᴱ H Γ M)
typeOfᴱ H Γ (function f ⟨ var x ∈ S ⟩∈ T is B end) = S ⇒ T
typeOfᴱ H Γ (block b is B end) = typeOfᴮ H Γ B

typeOfᴮ H Γ (function f ⟨ var x ∈ S ⟩∈ T is C end ∙ B) = typeOfᴮ H (Γ ⊕ f ↦ (S ⇒ T)) B
typeOfᴮ H Γ (local var x ∈ T ← M ∙ B) = typeOfᴮ H (Γ ⊕ x ↦ T) B
typeOfᴮ H Γ (return M ∙ B) = typeOfᴱ H Γ M
typeOfᴮ H Γ done = nil

contextOfᴱ : Heap yes → VarCtxt → Type → (Expr yes) → VarCtxt
contextOfᴮ : Heap yes → VarCtxt → Type → (Block yes) → VarCtxt

contextOfᴱ H Γ S nil = ∅
contextOfᴱ H Γ S (var x) = (x ↦ S)
contextOfᴱ H Γ S (addr a) = ∅
contextOfᴱ H Γ S (M $ N) = (contextOfᴱ H Γ (U ⇒ S) M) ⋒ (contextOfᴱ H Γ (src T) N) where T = typeOfᴱ H Γ M; U = typeOfᴱ H Γ N
contextOfᴱ H Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) = (contextOfᴮ H (Γ ⊕ x ↦ T) U B) ⊝ x
contextOfᴱ H Γ S (block b is B end) = (contextOfᴮ H Γ S B)

contextOfᴮ H Γ S (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) = ((contextOfᴮ H (Γ ⊕ x ↦ T) U C) ⊝ x) ⋒ ((contextOfᴮ H (Γ ⊕ f ↦ (T ⇒ U)) S B) ⊝ f)
contextOfᴮ H Γ S (local var x ∈ T ← M ∙ B) = (contextOfᴱ H Γ T M) ⋒ ((contextOfᴮ H (Γ ⊕ x ↦ T)S B) ⊝ x)
contextOfᴮ H Γ S (return M ∙ B) = (contextOfᴱ H Γ S M)
contextOfᴮ H Γ S done = ∅

typeCheckᴱ : ∀ H Γ S M → (Γ ⊢ᴱ S ∋ M ∈ (typeOfᴱ H Γ M) ⊣ (contextOfᴱ H Γ S M))
typeCheckᴮ : ∀ H Γ S B → (Γ ⊢ᴮ S ∋ B ∈ (typeOfᴮ H Γ B) ⊣ (contextOfᴮ H Γ S B))

typeCheckᴱ H Γ S nil = nil
typeCheckᴱ H Γ S (var x) = var x refl
typeCheckᴱ H Γ S (addr a) = addr a (typeOfᴴ (H [ a ]ᴴ))
typeCheckᴱ H Γ S (M $ N) = app (typeCheckᴱ H Γ (typeOfᴱ H Γ N ⇒ S) M) (typeCheckᴱ H Γ (src (typeOfᴱ H Γ M)) N)
typeCheckᴱ H Γ S (function f ⟨ var x ∈ T ⟩∈ U is B end) = function(typeCheckᴮ H (Γ ⊕ x ↦ T) U B)
typeCheckᴱ H Γ S (block b is B end) = block b (typeCheckᴮ H Γ S B)

typeCheckᴮ H Γ S (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) = function(typeCheckᴮ H (Γ ⊕ x ↦ T) U C) (typeCheckᴮ H (Γ ⊕ f ↦ (T ⇒ U)) S B)
typeCheckᴮ H Γ S (local var x ∈ T ← M ∙ B) = local (typeCheckᴱ H Γ T M) (typeCheckᴮ H (Γ ⊕ x ↦ T) S B)
typeCheckᴮ H Γ S (return M ∙ B) = return (typeCheckᴱ H Γ S M) (typeCheckᴮ H Γ nil B)
typeCheckᴮ H Γ S done = done
