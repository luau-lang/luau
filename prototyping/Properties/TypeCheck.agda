{-# OPTIONS --rewriting #-}

open import Luau.Type using (Mode)

module Properties.TypeCheck (m : Mode) where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either)
open import Luau.TypeCheck(m) using (_⊢ᴱ_∈_; _⊢ᴮ_∈_; nil; var; addr; app; function; block; done; return; local)
open import Luau.Syntax using (Block; Expr; yes; nil; var; addr; _$_; function_is_end; block_is_end; _∙_; return; done; local_←_; _⟨_⟩; _⟨_⟩∈_; var_∈_; name; fun; arg)
open import Luau.Type using (Type; nil; top; bot; _⇒_; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _↦_; _⊕_↦_; _⋒_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var; _≡ⱽ_)
open import Luau.Value using (Value; nil; addr; val)
open import Luau.Heap using (Heap; HeapValue; function_is_end) renaming (_[_] to _[_]ᴴ)
open import Properties.Dec using (yes; no)
open import Properties.Equality using (_≢_; sym; trans; cong)
open import Properties.Remember using (remember; _,_)

src : Type → Type
src = Luau.Type.src m

declaredTypeᴴ : Maybe(HeapValue yes) → Type
declaredTypeᴴ nothing = bot
declaredTypeᴴ (just function f ⟨ var x ∈ S ⟩∈ T is B end) = (S ⇒ T)

typeOfⱽ : Heap yes → Value → Type
typeOfⱽ H nil = nil
typeOfⱽ H (addr a) = declaredTypeᴴ (H [ a ]ᴴ)

typeOfᴱ : Heap yes → VarCtxt → (Expr yes) → Type
typeOfᴮ : Heap yes → VarCtxt → (Block yes) → Type

typeOfᴱ H Γ nil = nil
typeOfᴱ H Γ (var x) = Γ [ x ]ⱽ
typeOfᴱ H Γ (addr a) = declaredTypeᴴ (H [ a ]ᴴ)
typeOfᴱ H Γ (M $ N) = tgt(typeOfᴱ H Γ M)
typeOfᴱ H Γ (function f ⟨ var x ∈ S ⟩∈ T is B end) = S ⇒ T
typeOfᴱ H Γ (block b is B end) = typeOfᴮ H Γ B

typeOfᴮ H Γ (function f ⟨ var x ∈ S ⟩∈ T is C end ∙ B) = typeOfᴮ H (Γ ⊕ f ↦ (S ⇒ T)) B
typeOfᴮ H Γ (local var x ∈ T ← M ∙ B) = typeOfᴮ H (Γ ⊕ x ↦ T) B
typeOfᴮ H Γ (return M ∙ B) = typeOfᴱ H Γ M
typeOfᴮ H Γ done = nil

typeOfᴴ : Heap yes → Maybe(HeapValue yes) → Type
typeOfᴴ H nothing = bot
typeOfᴴ H (just function f ⟨ var x ∈ S ⟩∈ T is B end) = (S ⇒ typeOfᴮ H (x ↦ S) B)

typeOfᴱⱽ : ∀ {H Γ} v → (typeOfᴱ H Γ (val v) ≡ typeOfⱽ H v)
typeOfᴱⱽ nil = refl
typeOfᴱⱽ (addr a) = refl

typeCheckᴱ : ∀ H Γ M → (Γ ⊢ᴱ M ∈ (typeOfᴱ H Γ M))
typeCheckᴮ : ∀ H Γ B → (Γ ⊢ᴮ B ∈ (typeOfᴮ H Γ B))

typeCheckᴱ H Γ nil = nil
typeCheckᴱ H Γ (var x) = var x refl
typeCheckᴱ H Γ (addr a) = addr a (declaredTypeᴴ (H [ a ]ᴴ))
typeCheckᴱ H Γ (M $ N) = app (typeCheckᴱ H Γ M) (typeCheckᴱ H Γ N)
typeCheckᴱ H Γ (function f ⟨ var x ∈ T ⟩∈ U is B end) = function f (typeCheckᴮ H (Γ ⊕ x ↦ T) B)
typeCheckᴱ H Γ (block b is B end) = block b (typeCheckᴮ H Γ B)

typeCheckᴮ H Γ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) = function f (typeCheckᴮ H (Γ ⊕ x ↦ T) C) (typeCheckᴮ H (Γ ⊕ f ↦ (T ⇒ U)) B)
typeCheckᴮ H Γ (local var x ∈ T ← M ∙ B) = local (typeCheckᴱ H Γ M) (typeCheckᴮ H (Γ ⊕ x ↦ T) B)
typeCheckᴮ H Γ (return M ∙ B) = return (typeCheckᴱ H Γ M) (typeCheckᴮ H Γ B)
typeCheckᴮ H Γ done = done
