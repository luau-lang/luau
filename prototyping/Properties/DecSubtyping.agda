{-# OPTIONS --rewriting #-}

module Properties.DecSubtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src; tgt)
open import Properties.Contradiction using (CONTRADICTION; ¬)
open import Properties.Functions using (_∘_)

-- ¬Language T is the complement of Language T
language-comp : ∀ {T} t → ¬Language T t → ¬(Language T t)
language-comp t (p₁ , p₂) (left q) = language-comp t p₁ q
language-comp t (p₁ , p₂) (right q) = language-comp t p₂ q
language-comp t (left p) (q₁ , q₂) = language-comp t p q₁
language-comp t (right p) (q₁ , q₂) = language-comp t p q₂
language-comp (scalar s) (scalar-scalar s p₁ p₂) (scalar s) = p₂ refl
language-comp (scalar s) (function-scalar s) (scalar s) = language-comp function (scalar-function s) function
language-comp (scalar s) never (scalar ())
language-comp function (scalar-function ()) function
language-comp (function-ok t) (scalar-function-ok ()) (function-ok q)
language-comp (function-ok t) (function-ok p) (function-ok q) = language-comp t p q
language-comp (function-err t) (function-err p) (function-err q) = language-comp t q p

-- Properties of src
function-err-src : ∀ {T t} → (¬Language (src T) t) → Language T (function-err t)
function-err-src {T = nil} never = scalar-function-err nil
function-err-src {T = T₁ ⇒ T₂} p = function-err p
function-err-src {T = never} (scalar-scalar number () p)
function-err-src {T = never} (scalar-function-ok ())
function-err-src {T = unknown} never = unknown
function-err-src {T = boolean} p = scalar-function-err boolean
function-err-src {T = number} p = scalar-function-err number
function-err-src {T = string} p = scalar-function-err string
function-err-src {T = T₁ ∪ T₂} (left p) = left (function-err-src p)
function-err-src {T = T₁ ∪ T₂} (right p) = right (function-err-src p)
function-err-src {T = T₁ ∩ T₂} (p₁ , p₂) = function-err-src p₁ , function-err-src p₂

¬function-err-src : ∀ {T t} → (Language (src T) t) → ¬Language T (function-err t)
¬function-err-src {T = nil} (scalar ())
¬function-err-src {T = T₁ ⇒ T₂} p = function-err p
¬function-err-src {T = never} unknown = never
¬function-err-src {T = unknown} (scalar ())
¬function-err-src {T = boolean} (scalar ())
¬function-err-src {T = number} (scalar ())
¬function-err-src {T = string} (scalar ())
¬function-err-src {T = T₁ ∪ T₂} (p₁ , p₂) = (¬function-err-src p₁ , ¬function-err-src p₂)
¬function-err-src {T = T₁ ∩ T₂} (left p) = left (¬function-err-src p)
¬function-err-src {T = T₁ ∩ T₂} (right p) = right (¬function-err-src p)

src-¬function-err : ∀ {T t} → Language T (function-err t) → (¬Language (src T) t)
src-¬function-err {T = nil} p = never
src-¬function-err {T = T₁ ⇒ T₂} (function-err p) = p
src-¬function-err {T = never} (scalar-function-err ())
src-¬function-err {T = unknown} p = never
src-¬function-err {T = boolean} p = never
src-¬function-err {T = number} p = never
src-¬function-err {T = string} p = never
src-¬function-err {T = T₁ ∪ T₂} (left p) = left (src-¬function-err p)
src-¬function-err {T = T₁ ∪ T₂} (right p) = right (src-¬function-err p)
src-¬function-err {T = T₁ ∩ T₂} (p₁ , p₂) = (src-¬function-err p₁ , src-¬function-err p₂)

src-≮: : ∀ {T U} → (src T ≮: src U) → (U ≮: T)
src-≮: (witness t p q) = witness (function-err t) (function-err-src q) (¬function-err-src p)

-- Properties of tgt
tgt-function-ok : ∀ {T t} → (Language (tgt T) t) → Language T (function-ok t)
tgt-function-ok {T = nil} (scalar ())
tgt-function-ok {T = T₁ ⇒ T₂} p = function-ok p
tgt-function-ok {T = never} (scalar ())
tgt-function-ok {T = unknown} p = unknown
tgt-function-ok {T = boolean} (scalar ())
tgt-function-ok {T = number} (scalar ())
tgt-function-ok {T = string} (scalar ())
tgt-function-ok {T = T₁ ∪ T₂} (left p) = left (tgt-function-ok p)
tgt-function-ok {T = T₁ ∪ T₂} (right p) = right (tgt-function-ok p)
tgt-function-ok {T = T₁ ∩ T₂} (p₁ , p₂) = (tgt-function-ok p₁ , tgt-function-ok p₂)

function-ok-tgt : ∀ {T t} → Language T (function-ok t) → (Language (tgt T) t)
function-ok-tgt (function-ok p) = p
function-ok-tgt (left p) = left (function-ok-tgt p)
function-ok-tgt (right p) = right (function-ok-tgt p)
function-ok-tgt (p₁ , p₂) = (function-ok-tgt p₁ , function-ok-tgt p₂)
function-ok-tgt unknown = unknown

tgt-¬function-ok : ∀ {T t} → (¬Language (tgt T) t) → ¬Language T (function-ok t)
tgt-¬function-ok {T = nil} p = scalar-function-ok nil
tgt-¬function-ok {T = T₁ ⇒ T₂} p = function-ok p
tgt-¬function-ok {T = never} p = never
tgt-¬function-ok {T = unknown} (scalar-scalar s () p)
tgt-¬function-ok {T = unknown} (scalar-function ())
tgt-¬function-ok {T = unknown} (scalar-function-ok ())
tgt-¬function-ok {T = boolean} p = scalar-function-ok boolean
tgt-¬function-ok {T = number} p = scalar-function-ok number
tgt-¬function-ok {T = string} p = scalar-function-ok string
tgt-¬function-ok {T = T₁ ∪ T₂} (p₁ , p₂) = (tgt-¬function-ok p₁ , tgt-¬function-ok p₂)
tgt-¬function-ok {T = T₁ ∩ T₂} (left p) = left (tgt-¬function-ok p)
tgt-¬function-ok {T = T₁ ∩ T₂} (right p) = right (tgt-¬function-ok p)

tgt-≮: : ∀ {T U} → (tgt T ≮: tgt U) → (T ≮: U)
tgt-≮: (witness t p q) = witness (function-ok t) (tgt-function-ok p) (tgt-¬function-ok q)

-- Language membership is decidable
dec-language : ∀ T t → Either (¬Language T t) (Language T t)
dec-language nil (scalar number) = Left (scalar-scalar number nil (λ ()))
dec-language nil (scalar boolean) = Left (scalar-scalar boolean nil (λ ()))
dec-language nil (scalar string) = Left (scalar-scalar string nil (λ ()))
dec-language nil (scalar nil) = Right (scalar nil)
dec-language nil function = Left (scalar-function nil)
dec-language nil (function-ok t) = Left (scalar-function-ok nil)
dec-language nil (function-err t) = Right (scalar-function-err nil)
dec-language boolean (scalar number) = Left (scalar-scalar number boolean (λ ()))
dec-language boolean (scalar boolean) = Right (scalar boolean)
dec-language boolean (scalar string) = Left (scalar-scalar string boolean (λ ()))
dec-language boolean (scalar nil) = Left (scalar-scalar nil boolean (λ ()))
dec-language boolean function = Left (scalar-function boolean)
dec-language boolean (function-ok t) = Left (scalar-function-ok boolean)
dec-language boolean (function-err t) = Right (scalar-function-err boolean)
dec-language number (scalar number) = Right (scalar number)
dec-language number (scalar boolean) = Left (scalar-scalar boolean number (λ ()))
dec-language number (scalar string) = Left (scalar-scalar string number (λ ()))
dec-language number (scalar nil) = Left (scalar-scalar nil number (λ ()))
dec-language number function = Left (scalar-function number)
dec-language number (function-ok t) = Left (scalar-function-ok number)
dec-language number (function-err t) = Right (scalar-function-err number)
dec-language string (scalar number) = Left (scalar-scalar number string (λ ()))
dec-language string (scalar boolean) = Left (scalar-scalar boolean string (λ ()))
dec-language string (scalar string) = Right (scalar string)
dec-language string (scalar nil) = Left (scalar-scalar nil string (λ ()))
dec-language string function = Left (scalar-function string)
dec-language string (function-ok t) = Left (scalar-function-ok string)
dec-language string (function-err t) = Right (scalar-function-err string)
dec-language (T₁ ⇒ T₂) (scalar s) = Left (function-scalar s)
dec-language (T₁ ⇒ T₂) function = Right function
dec-language (T₁ ⇒ T₂) (function-ok t) = mapLR function-ok function-ok (dec-language T₂ t)
dec-language (T₁ ⇒ T₂) (function-err t) = mapLR function-err function-err (swapLR (dec-language T₁ t))
dec-language never t = Left never
dec-language unknown t = Right unknown
dec-language (T₁ ∪ T₂) t = cond (λ p → cond (Left ∘ _,_ p) (Right ∘ right) (dec-language T₂ t)) (Right ∘ left) (dec-language T₁ t)
dec-language (T₁ ∩ T₂) t = cond (Left ∘ left) (λ p → cond (Left ∘ right) (Right ∘ _,_ p) (dec-language T₂ t)) (dec-language T₁ t)

-- if T <: U then ¬Language U ⊆ ¬Language T
<:-impl-⊇ : ∀ {T U} → (T <: U) → ∀ t → ¬Language U t → ¬Language T t
<:-impl-⊇ {T} p t ¬Ut with dec-language T t
<:-impl-⊇ p t ¬Ut | Left ¬Tt = ¬Tt
<:-impl-⊇ p t ¬Ut | Right Tt = CONTRADICTION (language-comp t ¬Ut (p t Tt))

-- Subtyping is decidable
-- Honest, this terminates (because src T and tgt T decrease the depth of the type)

{-# TERMINATING #-}
dec-subtyping : ∀ T U → Either (T ≮: U) (T <: U)
dec-subtyping T U = result where

  P : Tree → Set
  P t = Either (¬Language T t) (Language U t)

  Q : Tree → Set
  Q t = Either (T ≮: U) (P t)

  decQ : ∀ t → Q t
  decQ t with dec-language T t | dec-language U t
  decQ t | Left ¬Tt | _ = Right (Left ¬Tt)
  decQ t | Right Tt | Left ¬Ut = Left (witness t Tt ¬Ut)
  decQ t | Right _ | Right Ut = Right (Right Ut)

  lemma : P(scalar number) → P(scalar boolean) → P(scalar nil) → P(scalar string) → P(function) → (src U <: src T) → (tgt T <: tgt U) → (T <: U)
  lemma (Left ¬Tt) boolP nilP stringP funP srcy tgty (scalar number) Tt = CONTRADICTION (language-comp (scalar number) ¬Tt Tt)
  lemma (Right Ut) boolP nilP stringP funP srcy tgty (scalar number) Tt = Ut
  lemma numP (Left ¬Tt) nilP stringP funP srcy tgty (scalar boolean) Tt = CONTRADICTION (language-comp (scalar boolean) ¬Tt Tt)
  lemma numP (Right Ut) nilP stringP funP srcy tgty (scalar boolean) Tt = Ut
  lemma numP boolP (Left ¬Tt) stringP funP srcy tgty (scalar nil) Tt = CONTRADICTION (language-comp (scalar nil) ¬Tt Tt)
  lemma numP boolP (Right Ut) stringP funP srcy tgty (scalar nil) Tt = Ut
  lemma numP boolP nilP (Left ¬Tt) funP srcy tgty (scalar string) Tt = CONTRADICTION (language-comp (scalar string) ¬Tt Tt)
  lemma numP boolP nilP (Right Ut) funP srcy tgty (scalar string) Tt = Ut
  lemma numP boolP nilP stringP (Left ¬Tt) srcy tgty function Tt = CONTRADICTION (language-comp function ¬Tt Tt)
  lemma numP boolP nilP stringP (Right Ut) srcy tgty function Tt = Ut
  lemma numP boolP nilP stringP funP srcy tgty (function-ok t) Tt = tgt-function-ok (tgty t (function-ok-tgt Tt))
  lemma numP boolP nilP stringP funP srcy tgty (function-err t) Tt = function-err-src (<:-impl-⊇ srcy t (src-¬function-err Tt))

  result : Either (T ≮: U) (T <: U)
  result with decQ (scalar number)
  result | Left r = Left r
  result | Right numP with decQ (scalar boolean)
  result | Right numP | Left r = Left r
  result | Right numP | Right boolP with decQ (scalar nil)
  result | Right numP | Right boolP | Left r = Left r
  result | Right numP | Right boolP | Right nilP with decQ (scalar string)
  result | Right numP | Right boolP | Right nilP | Left r = Left r
  result | Right numP | Right boolP | Right nilP | Right strP with decQ (function)
  result | Right numP | Right boolP | Right nilP | Right strP | Left r = Left r
  result | Right numP | Right boolP | Right nilP | Right strP | Right funP with dec-subtyping (src U) (src T)
  result | Right numP | Right boolP | Right nilP | Right strP | Right funP | Left r = Left (src-≮: r)
  result | Right numP | Right boolP | Right nilP | Right strP | Right funP | Right srcy with dec-subtyping (tgt T) (tgt U)
  result | Right numP | Right boolP | Right nilP | Right strP | Right funP | Right srcy | Left r = Left (tgt-≮: r)
  result | Right numP | Right boolP | Right nilP | Right strP | Right funP | Right srcy | Right tgty = Right (lemma numP boolP nilP strP funP srcy tgty)
