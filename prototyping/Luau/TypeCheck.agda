{-# OPTIONS --rewriting #-}

module Luau.TypeCheck where

open import Agda.Builtin.Equality using (_≡_)
open import FFI.Data.Either using (Either; Left; Right)
open import FFI.Data.Maybe using (Maybe; just)
open import Luau.Syntax using (Expr; Stat; Block; BinaryOperator; yes; nil; addr; number; bool; string; val; var; var_∈_; _⟨_⟩∈_; function_is_end; _$_; block_is_end; binexp; local_←_; _∙_; done; return; name; +; -; *; /; <; >; ==; ~=; <=; >=; ··)
open import Luau.Var using (Var)
open import Luau.Addr using (Addr)
open import Luau.Heap using (Heap; Object; function_is_end) renaming (_[_] to _[_]ᴴ)
open import Luau.Subtyping using (_≮:_; _<:_)
open import Luau.Type using (Type; nil; never; unknown; number; boolean; string; _⇒_; _∪_; _∩_; src; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _⋒_; _↦_; _⊕_↦_; _⊝_) renaming (_[_] to _[_]ⱽ)
open import FFI.Data.Vector using (Vector)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Properties.DecSubtyping using (dec-subtyping)
open import Properties.Product using (_×_; _,_)

orUnknown : Maybe Type → Type
orUnknown nothing = unknown
orUnknown (just T) = T

srcBinOp : BinaryOperator → Type
srcBinOp + = number
srcBinOp - = number
srcBinOp * = number
srcBinOp / = number
srcBinOp < = number
srcBinOp > = number
srcBinOp == = unknown
srcBinOp ~= = unknown
srcBinOp <= = number
srcBinOp >= = number
srcBinOp ·· = string

tgtBinOp : BinaryOperator → Type
tgtBinOp + = number
tgtBinOp - = number
tgtBinOp * = number
tgtBinOp / = number
tgtBinOp < = boolean
tgtBinOp > = boolean
tgtBinOp == = boolean
tgtBinOp ~= = boolean
tgtBinOp <= = boolean
tgtBinOp >= = boolean
tgtBinOp ·· = string

-- (F ⋓ G) is a function type whose domain is the union of F's and G's domain
-- and whose target is the union of F's and G's target.
_⋓_ : Type → Type → Type
(S ⇒ T) ⋓ (U ⇒ V) = (S ∪ U) ⇒ (T ∪ V)
(S ⇒ T) ⋓ (U₁ ∪ U₂) = ((S ⇒ T) ⋓ U₁) ∪ ((S ⇒ T) ⋓ U₂)
(S ⇒ T) ⋓ (U₁ ∩ U₂) = ((S ⇒ T) ⋓ U₁) ∩ ((S ⇒ T) ⋓ U₂)
(S ⇒ T) ⋓ unknown = unknown
(S ⇒ T) ⋓ nil = (S ⇒ T)
(S ⇒ T) ⋓ never = (S ⇒ T)
(S ⇒ T) ⋓ boolean = (S ⇒ T)
(S ⇒ T) ⋓ number = (S ⇒ T)
(S ⇒ T) ⋓ string = (S ⇒ T)
(T₁ ∪ T₂) ⋓ U = (T₁ ⋓ U) ∪ (T₂ ⋓ U)
(T₁ ∩ T₂) ⋓ U = (T₁ ⋓ U) ∩ (T₂ ⋓ U)
unknown ⋓ U = unknown
nil ⋓ U = U
never ⋓ U = U
boolean ⋓ U = U
number ⋓ U = U
string ⋓ U = U

-- resolve F V is the result of applying a function of type F
-- to an argument of type V. This does function overload resolution,
-- e.g. `resolve (((number) -> string) & ((string) -> number)) (number)` is `string`.

resolveFun : ∀{S V} → Either (V ≮: S) (V <: S) → Type → Type
resolveFun (Left p) T = unknown
resolveFun (Right p) T = T

-- Honest this terminates, since each recursive call has
-- fewer intersections, and otherwise we proceed by structural induction.
{-# TERMINATING #-}
resolve : Type → Type → Type
resolve nil V = never
resolve (S ⇒ T) V = resolveFun (dec-subtyping V S) T
resolve never V = never
resolve unknown V = unknown
resolve boolean V = never
resolve number V = never
resolve string V = never
resolve (F ∪ G) V = (resolve F V) ∪ (resolve G V)
resolve (F ∩ G) V = ((resolve F V) ∩ (resolve G V)) ∩ (resolve (F ⋓ G) V)

data _⊢ᴮ_∈_ : VarCtxt → Block yes → Type → Set
data _⊢ᴱ_∈_ : VarCtxt → Expr yes → Type → Set

data _⊢ᴮ_∈_ where

  done : ∀ {Γ} →

    ---------------
    Γ ⊢ᴮ done ∈ nil

  return : ∀ {M B T U Γ} →

    Γ ⊢ᴱ M ∈ T →
    Γ ⊢ᴮ B ∈ U →
    ---------------------
    Γ ⊢ᴮ return M ∙ B ∈ T

  local : ∀ {x M B T U V Γ} →

    Γ ⊢ᴱ M ∈ U →
    (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V →
    --------------------------------
    Γ ⊢ᴮ local var x ∈ T ← M ∙ B ∈ V

  function : ∀ {f x B C T U V W Γ} →

    (Γ ⊕ x ↦ T) ⊢ᴮ C ∈ V →
    (Γ ⊕ f ↦ (T ⇒ U)) ⊢ᴮ B ∈ W →
    -------------------------------------------------
    Γ ⊢ᴮ function f ⟨ var x ∈ T ⟩∈ U is C end ∙ B ∈ W

data _⊢ᴱ_∈_ where

  nil : ∀ {Γ} →

    --------------------
    Γ ⊢ᴱ (val nil) ∈ nil

  var : ∀ {x T Γ} →

    T ≡ orUnknown(Γ [ x ]ⱽ) →
    ----------------
    Γ ⊢ᴱ (var x) ∈ T

  addr : ∀ {a Γ} T →

    -----------------
    Γ ⊢ᴱ val(addr a) ∈ T

  number : ∀ {n Γ} →

    ---------------------------
    Γ ⊢ᴱ val(number n) ∈ number

  bool : ∀ {b Γ} →

    --------------------------
    Γ ⊢ᴱ val(bool b) ∈ boolean
  
  string : ∀ {x Γ} →

    ---------------------------
    Γ ⊢ᴱ val(string x) ∈ string

  app : ∀ {M N T U Γ} →

    Γ ⊢ᴱ M ∈ T →
    Γ ⊢ᴱ N ∈ U →
    ----------------------------
    Γ ⊢ᴱ (M $ N) ∈ (resolve T U)

  function : ∀ {f x B T U V Γ} →

    (Γ ⊕ x ↦ T) ⊢ᴮ B ∈ V →
    -----------------------------------------------------
    Γ ⊢ᴱ (function f ⟨ var x ∈ T ⟩∈ U is B end) ∈ (T ⇒ U)

  block : ∀ {b B T U Γ} →

    Γ ⊢ᴮ B ∈ U →
    ------------------------------------
    Γ ⊢ᴱ (block var b ∈ T is B end) ∈ T

  binexp : ∀ {op Γ M N T U} →

    Γ ⊢ᴱ M ∈ T →
    Γ ⊢ᴱ N ∈ U →
    ----------------------------------
    Γ ⊢ᴱ (binexp M op N) ∈ tgtBinOp op

data ⊢ᴼ_ : Maybe(Object yes) → Set where

  nothing :

    ---------
    ⊢ᴼ nothing

  function : ∀ {f x T U V B} →

    (x ↦ T) ⊢ᴮ B ∈ V →
    ----------------------------------------------
    ⊢ᴼ (just function f ⟨ var x ∈ T ⟩∈ U is B end)

⊢ᴴ_ : Heap yes → Set
⊢ᴴ H = ∀ a {O} → (H [ a ]ᴴ ≡ O) → (⊢ᴼ O)

_⊢ᴴᴱ_▷_∈_ : VarCtxt → Heap yes → Expr yes → Type → Set
(Γ ⊢ᴴᴱ H ▷ M ∈ T) = (⊢ᴴ H) × (Γ ⊢ᴱ M ∈ T)

_⊢ᴴᴮ_▷_∈_ : VarCtxt → Heap yes → Block yes → Type → Set
(Γ ⊢ᴴᴮ H ▷ B ∈ T) = (⊢ᴴ H) × (Γ ⊢ᴮ B ∈ T)
