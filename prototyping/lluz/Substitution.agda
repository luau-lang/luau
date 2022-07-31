module Luau.Substitution where

open import Luau.Syntax using (Value; Expr; Stat; Block; val; nil; bool; addr; var; function_is_end; _$_; block_is_end; local_←_; _∙_; done; return; _⟨_⟩ ; name; fun; arg; number; binexp)
open import Luau.Var using (Var; _≡ⱽ_)
open import Properties.Dec using (Dec; yes; no)

_[_/_]ᴱ : ∀ {a} → Expr a → Value → Var → Expr a
_[_/_]ᴮ : ∀ {a} → Block a → Value → Var → Block a
var_[_/_]ᴱwhenever_ : ∀ {a P} → Var → Value → Var → (Dec P) → Expr a
_[_/_]ᴮunless_ : ∀ {a P} → Block a → Value → Var → (Dec P) → Block a

val w [ v / x ]ᴱ = val w
var y [ v / x ]ᴱ = var y [ v / x ]ᴱwhenever (x ≡ⱽ y)
(M $ N) [ v / x ]ᴱ = (M [ v / x ]ᴱ) $ (N [ v / x ]ᴱ)
function F is C end [ v / x ]ᴱ = function F is C [ v / x ]ᴮunless (x ≡ⱽ name(arg F)) end
block b is C end [ v / x ]ᴱ = block b is C [ v / x ]ᴮ end
(binexp e₁ op e₂) [ v / x ]ᴱ = binexp (e₁ [ v / x ]ᴱ) op (e₂ [ v / x ]ᴱ)

(function F is C end ∙ B) [ v / x ]ᴮ = function F is (C [ v / x ]ᴮunless (x ≡ⱽ name(arg F))) end ∙ (B [ v / x ]ᴮunless (x ≡ⱽ name(fun F)))
(local y ← M ∙ B) [ v / x ]ᴮ = local y ← (M [ v / x ]ᴱ) ∙ (B [ v / x ]ᴮunless (x ≡ⱽ name y))
(return M ∙ B) [ v / x ]ᴮ = return (M [ v / x ]ᴱ) ∙ (B [ v / x ]ᴮ)
done [ v / x ]ᴮ = done

var y [ v / x ]ᴱwhenever yes p = val v
var y [ v / x ]ᴱwhenever no p = var y

B [ v / x ]ᴮunless yes p = B
B [ v / x ]ᴮunless no p = B [ v / x ]ᴮ
