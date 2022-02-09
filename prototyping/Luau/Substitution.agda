module Luau.Substitution where

open import Luau.Syntax using (Expr; Stat; Block; nil; addr; var; function⟨_⟩_end; _$_; block_end; local_←_; _∙_; done; function_⟨_⟩_end; return)
open import Luau.Value using (Value; val)
open import Luau.Var using (Var; _≡ⱽ_)
open import Properties.Dec using (Dec; yes; no)

_[_/_]ᴱ : Expr → Value → Var → Expr
_[_/_]ᴮ : Block → Value → Var → Block
var_[_/_]ᴱwhenever_ : ∀ {P} → Var → Value → Var → (Dec P) → Expr
_[_/_]ᴮunless_ : ∀ {P} → Block → Value → Var → (Dec P) → Block

nil [ v / x ]ᴱ = nil
var y [ v / x ]ᴱ = var y [ v / x ]ᴱwhenever (x ≡ⱽ y)
addr a [ v / x ]ᴱ = addr a
(M $ N) [ v / x ]ᴱ = (M [ v / x ]ᴱ) $ (N [ v / x ]ᴱ)
function⟨ y ⟩ C end [ v / x ]ᴱ = function⟨ y ⟩ C [ v / x ]ᴮunless (x ≡ⱽ y) end
block C end [ v / x ]ᴱ = block C [ v / x ]ᴮ end

(function f ⟨ y ⟩ C end ∙ B) [ v / x ]ᴮ = function f ⟨ y ⟩ (C [ v / x ]ᴮunless (x ≡ⱽ y)) end ∙ (B [ v / x ]ᴮunless (x ≡ⱽ f))
(local y ← M ∙ B) [ v / x ]ᴮ = local y ← (M [ v / x ]ᴱ) ∙ (B [ v / x ]ᴮunless (x ≡ⱽ y))
(return M ∙ B) [ v / x ]ᴮ = return (M [ v / x ]ᴱ) ∙ (B [ v / x ]ᴮ)
done [ v / x ]ᴮ = done

var y [ v / x ]ᴱwhenever yes p = val v
var y [ v / x ]ᴱwhenever no p = var y

B [ v / x ]ᴮunless yes p = B
B [ v / x ]ᴮunless no p = B [ v / x ]ᴮ

