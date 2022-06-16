{-# OPTIONS --rewriting #-}

module Properties.TypeCheck where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Bool using (Bool; true; false)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either)
open import Luau.ResolveOverloads using (resolve)
open import Luau.TypeCheck using (_⊢ᴱ_∈_; _⊢ᴮ_∈_; ⊢ᴼ_; ⊢ᴴ_; _⊢ᴴᴱ_▷_∈_; _⊢ᴴᴮ_▷_∈_; nil; var; addr; number; bool; string; app; function; block; binexp; done; return; local; nothing; orUnknown; tgtBinOp)
open import Luau.Syntax using (Block; Expr; Value; BinaryOperator; yes; nil; addr; number; bool; string; val; var; binexp; _$_; function_is_end; block_is_end; _∙_; return; done; local_←_; _⟨_⟩; _⟨_⟩∈_; var_∈_; name; fun; arg; +; -; *; /; <; >; ==; ~=; <=; >=)
open import Luau.Type using (Type; nil; unknown; never; number; boolean; string; _⇒_)
open import Luau.RuntimeType using (RuntimeType; nil; number; function; string; valueType)
open import Luau.VarCtxt using (VarCtxt; ∅; _↦_; _⊕_↦_; _⋒_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var; _≡ⱽ_)
open import Luau.Heap using (Heap; Object; function_is_end) renaming (_[_] to _[_]ᴴ)
open import Properties.Contradiction using (CONTRADICTION)
open import Properties.Dec using (yes; no)
open import Properties.Equality using (_≢_; sym; trans; cong)
open import Properties.Product using (_×_; _,_)
open import Properties.Remember using (Remember; remember; _,_)

typeOfᴼ : Object yes → Type
typeOfᴼ (function f ⟨ var x ∈ S ⟩∈ T is B end) = (S ⇒ T)

typeOfᴹᴼ : Maybe(Object yes) → Maybe Type
typeOfᴹᴼ nothing = nothing
typeOfᴹᴼ (just O) = just (typeOfᴼ O)

typeOfⱽ : Heap yes → Value → Maybe Type
typeOfⱽ H nil = just nil
typeOfⱽ H (bool b) = just boolean
typeOfⱽ H (addr a) = typeOfᴹᴼ (H [ a ]ᴴ)
typeOfⱽ H (number n) = just number
typeOfⱽ H (string x) = just string

typeOfᴱ : Heap yes → VarCtxt → (Expr yes) → Type
typeOfᴮ : Heap yes → VarCtxt → (Block yes) → Type

typeOfᴱ H Γ (var x) = orUnknown(Γ [ x ]ⱽ)
typeOfᴱ H Γ (val v) = orUnknown(typeOfⱽ H v)
typeOfᴱ H Γ (M $ N) = resolve (typeOfᴱ H Γ M) (typeOfᴱ H Γ N)
typeOfᴱ H Γ (function f ⟨ var x ∈ S ⟩∈ T is B end) = S ⇒ T
typeOfᴱ H Γ (block var b ∈ T is B end) = T
typeOfᴱ H Γ (binexp M op N) = tgtBinOp op

typeOfᴮ H Γ (function f ⟨ var x ∈ S ⟩∈ T is C end ∙ B) = typeOfᴮ H (Γ ⊕ f ↦ (S ⇒ T)) B
typeOfᴮ H Γ (local var x ∈ T ← M ∙ B) = typeOfᴮ H (Γ ⊕ x ↦ T) B
typeOfᴮ H Γ (return M ∙ B) = typeOfᴱ H Γ M
typeOfᴮ H Γ done = nil

mustBeNumber : ∀ H Γ v → (typeOfᴱ H Γ (val v) ≡ number) → (valueType(v) ≡ number)
mustBeNumber H Γ (addr a) p with remember (H [ a ]ᴴ)
mustBeNumber H Γ (addr a) p | (just O , q) with trans (cong orUnknown (cong typeOfᴹᴼ (sym q))) p
mustBeNumber H Γ (addr a) p | (just function f ⟨ var x ∈ T ⟩∈ U is B end , q) | ()
mustBeNumber H Γ (addr a) p | (nothing , q) with trans (cong orUnknown (cong typeOfᴹᴼ (sym q))) p
mustBeNumber H Γ (addr a) p | nothing , q | ()
mustBeNumber H Γ (number n) p = refl

mustBeString : ∀ H Γ v → (typeOfᴱ H Γ (val v) ≡ string) → (valueType(v) ≡ string)
mustBeString H Γ (addr a) p with remember (H [ a ]ᴴ)
mustBeString H Γ (addr a) p | (just O , q) with trans (cong orUnknown (cong typeOfᴹᴼ (sym q))) p
mustBeString H Γ (addr a) p | (just function f ⟨ var x ∈ T ⟩∈ U is B end , q) | ()
mustBeString H Γ (addr a) p | (nothing , q) with trans (cong orUnknown (cong typeOfᴹᴼ (sym q))) p
mustBeString H Γ (addr a) p | (nothing , q) | ()
mustBeString H Γ (string x) p = refl

typeCheckᴱ : ∀ H Γ M → (Γ ⊢ᴱ M ∈ (typeOfᴱ H Γ M))
typeCheckᴮ : ∀ H Γ B → (Γ ⊢ᴮ B ∈ (typeOfᴮ H Γ B))

typeCheckᴱ H Γ (var x) = var refl
typeCheckᴱ H Γ (val nil) = nil
typeCheckᴱ H Γ (val (addr a)) = addr (orUnknown (typeOfᴹᴼ (H [ a ]ᴴ)))
typeCheckᴱ H Γ (val (number n)) = number
typeCheckᴱ H Γ (val (bool b)) = bool
typeCheckᴱ H Γ (val (string x)) = string
typeCheckᴱ H Γ (M $ N) = app (typeCheckᴱ H Γ M) (typeCheckᴱ H Γ N)
typeCheckᴱ H Γ (function f ⟨ var x ∈ T ⟩∈ U is B end) = function (typeCheckᴮ H (Γ ⊕ x ↦ T) B)
typeCheckᴱ H Γ (block var b ∈ T is B end) = block (typeCheckᴮ H Γ B)
typeCheckᴱ H Γ (binexp M op N) = binexp (typeCheckᴱ H Γ M) (typeCheckᴱ H Γ N)

typeCheckᴮ H Γ (function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B) = function (typeCheckᴮ H (Γ ⊕ x ↦ T) C) (typeCheckᴮ H (Γ ⊕ f ↦ (T ⇒ U)) B)
typeCheckᴮ H Γ (local var x ∈ T ← M ∙ B) = local (typeCheckᴱ H Γ M) (typeCheckᴮ H (Γ ⊕ x ↦ T) B)
typeCheckᴮ H Γ (return M ∙ B) = return (typeCheckᴱ H Γ M) (typeCheckᴮ H Γ B)
typeCheckᴮ H Γ done = done

typeCheckᴼ : ∀ H O → (⊢ᴼ O)
typeCheckᴼ H nothing = nothing
typeCheckᴼ H (just function f ⟨ var x ∈ T ⟩∈ U is B end) = function (typeCheckᴮ H (x ↦ T) B)

typeCheckᴴ : ∀ H → (⊢ᴴ H)
typeCheckᴴ H a {O} p = typeCheckᴼ H (O)

typeCheckᴴᴱ : ∀ H Γ M → (Γ ⊢ᴴᴱ H ▷ M ∈ typeOfᴱ H Γ M)
typeCheckᴴᴱ H Γ M = (typeCheckᴴ H , typeCheckᴱ H Γ M)

typeCheckᴴᴮ : ∀ H Γ M → (Γ ⊢ᴴᴮ H ▷ M ∈ typeOfᴮ H Γ M)
typeCheckᴴᴮ H Γ M = (typeCheckᴴ H , typeCheckᴮ H Γ M)
 
