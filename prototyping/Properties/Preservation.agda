open import Luau.Type using (Mode)

module Properties.Preservation (m : Mode) where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import FFI.Data.Either using (Either)
open import Luau.TypeCheck(m) using (_▷_⊢ᴱ_∋_∈_⊣_; _▷_⊢ᴮ_∋_∈_⊣_; _▷_✓; nil; var; addr; app; function; block; done; return; local)
open import Luau.Syntax using (Block; Expr; yes; nil; var; addr; _$_; function_is_end; block_is_end; _∙_; return; done; local_←_; _⟨_⟩; _⟨_⟩∈_; var_∈_; name; fun; arg)
open import Luau.Type using (Type; nil; top; _⇒_; tgt)
open import Luau.VarCtxt using (VarCtxt; ∅; _↦_; _⊕_↦_; _⋒_; _⊝_; ⊕-[]) renaming (_[_] to _[_]ⱽ)
open import Luau.Addr using (Addr)
open import Luau.Var using (Var; _≡ⱽ_)
open import Luau.AddrCtxt using (AddrCtxt) renaming (_[_] to _[_]ᴬ)
open import Luau.Heap using (Heap)
open import Luau.OpSem using (_⊢_⟶ᴱ_⊣_)
open import Properties.Dec using (yes; no)
open import Properties.Equality using (_≢_; sym; trans; cong)
open import Properties.Remember using (remember; _,_)

src : Type → Type
src = Luau.Type.src m

preservationᴱ : ∀ {Σ Γ S Δ T H H′ M M′} → (Σ ▷ H ✓) → (Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ) → (H ⊢ M ⟶ᴱ M′ ⊣ H′) → (Σ ▷ Γ ⊢ᴱ S ∋ M ∈ T ⊣ Δ)
preservationᴱ = ?
