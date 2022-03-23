{-# OPTIONS --rewriting #-}

module Properties.Subtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; any; none; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; strict; nil; number; string; boolean; none; any; _⇒_; _∪_; _∩_; tgt)
open import Properties.Contradiction using (CONTRADICTION; ¬)
open import Properties.Equality using (_≢_)
open import Properties.Functions using (_∘_)

src = Luau.Type.src strict

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
dec-language none t = Left none
dec-language any t = Right any
dec-language (T₁ ∪ T₂) t = cond (λ p → cond (Left ∘ _,_ p) (Right ∘ right) (dec-language T₂ t)) (Right ∘ left) (dec-language T₁ t)
dec-language (T₁ ∩ T₂) t = cond (Left ∘ left) (λ p → cond (Left ∘ right) (Right ∘ _,_ p) (dec-language T₂ t)) (dec-language T₁ t)

-- ¬Language T is the complement of Language T
language-comp : ∀ {T} t → ¬Language T t → ¬(Language T t)
language-comp t (p₁ , p₂) (left q) = language-comp t p₁ q
language-comp t (p₁ , p₂) (right q) = language-comp t p₂ q
language-comp t (left p) (q₁ , q₂) = language-comp t p q₁
language-comp t (right p) (q₁ , q₂) = language-comp t p q₂
language-comp (scalar s) (scalar-scalar s p₁ p₂) (scalar s) = p₂ refl
language-comp (scalar s) (function-scalar s) (scalar s) = language-comp function (scalar-function s) function
language-comp (scalar s) none (scalar ())
language-comp function (scalar-function ()) function
language-comp (function-ok t) (scalar-function-ok ()) (function-ok q)
language-comp (function-ok t) (function-ok p) (function-ok q) = language-comp t p q
language-comp (function-err t) (function-err p) (function-err q) = language-comp t q p

-- ≮: is the complement of <:
¬≮:-impl-<: : ∀ {T U} → ¬(T ≮: U) → (T <: U)
¬≮:-impl-<: {T} {U} p t q with dec-language U t
¬≮:-impl-<: {T} {U} p t q | Left r = CONTRADICTION (p (witness t q r))
¬≮:-impl-<: {T} {U} p t q | Right r = r

<:-impl-¬≮: : ∀ {T U} → (T <: U) → ¬(T ≮: U)
<:-impl-¬≮: p (witness t q r) = language-comp t r (p t q)

-- reflexivity
≮:-refl : ∀ {T} → ¬(T ≮: T)
≮:-refl (witness t p q) = language-comp t q p

<:-refl : ∀ {T} → (T <: T)
<:-refl = ¬≮:-impl-<: ≮:-refl

-- transititivity
≮:-trans-≡ : ∀ {S T U} → (S ≮: T) → (T ≡ U) → (S ≮: U)
≮:-trans-≡ p refl = p

≡-trans-≮: : ∀ {S T U} → (S ≡ T) → (T ≮: U) → (S ≮: U)
≡-trans-≮: refl p = p

≮:-trans : ∀ {S T U} → (S ≮: U) → Either (S ≮: T) (T ≮: U)
≮:-trans {T = T} (witness t p q) = mapLR (witness t p) (λ z → witness t z q) (dec-language T t)

<:-trans : ∀ {S T U} → (S <: T) → (T <: U) → (S <: U)
<:-trans p q = ¬≮:-impl-<: (cond (<:-impl-¬≮: p) (<:-impl-¬≮: q) ∘ ≮:-trans)

-- Properties of scalars
skalar = number ∪ (string ∪ (nil ∪ boolean))

function-≮:-scalar : ∀ {S T U} → (Scalar U) → ((S ⇒ T) ≮: U)
function-≮:-scalar s = witness function function (scalar-function s)

scalar-≮:-function : ∀ {S T U} → (Scalar U) → (U ≮: (S ⇒ T))
scalar-≮:-function s = witness (scalar s) (scalar s) (function-scalar s)

any-≮:-scalar : ∀ {U} → (Scalar U) → (any ≮: U)
any-≮:-scalar s = witness (function-ok (scalar s)) any (scalar-function-ok s)

scalar-≮:-none : ∀ {U} → (Scalar U) → (U ≮: none)
scalar-≮:-none s = witness (scalar s) (scalar s) none

scalar-≢-impl-≮: : ∀ {T U} → (Scalar T) → (Scalar U) → (T ≢ U) → (T ≮: U)
scalar-≢-impl-≮: s₁ s₂ p = witness (scalar s₁) (scalar s₁) (scalar-scalar s₁ s₂ p)

-- Properties of tgt
tgt-function-ok : ∀ {T t} → (Language (tgt T) t) → Language T (function-ok t)
tgt-function-ok {T = nil} (scalar ())
tgt-function-ok {T = T₁ ⇒ T₂} p = function-ok p
tgt-function-ok {T = none} (scalar ())
tgt-function-ok {T = any} p = any
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
function-ok-tgt any = any

skalar-function-ok : ∀ {t} → (¬Language skalar (function-ok t))
skalar-function-ok = (scalar-function-ok number , (scalar-function-ok string , (scalar-function-ok nil , scalar-function-ok boolean)))

skalar-scalar : ∀ {T} (s : Scalar T) → (Language skalar (scalar s))
skalar-scalar number = left (scalar number)
skalar-scalar boolean = right (right (right (scalar boolean)))
skalar-scalar string = right (left (scalar string))
skalar-scalar nil = right (right (left (scalar nil)))

tgt-none-≮: : ∀ {T U} → (tgt T ≮: U) → (T ≮: (skalar ∪ (none ⇒ U)))
tgt-none-≮: (witness t p q) = witness (function-ok t) (tgt-function-ok p) (skalar-function-ok , function-ok q)

none-tgt-≮: : ∀ {T U} → (T ≮: (skalar ∪ (none ⇒ U))) → (tgt T ≮: U)
none-tgt-≮: (witness (scalar s) p (q₁ , q₂)) = CONTRADICTION (≮:-refl (witness (scalar s) (skalar-scalar s) q₁))
none-tgt-≮: (witness function p (q₁ , scalar-function ()))
none-tgt-≮: (witness (function-ok t) p (q₁ , function-ok q₂)) = witness t (function-ok-tgt p) q₂
none-tgt-≮: (witness (function-err (scalar s)) p (q₁ , function-err (scalar ())))

-- Properties of src
function-err-src : ∀ {T t} → (¬Language (src T) t) → Language T (function-err t)
function-err-src {T = nil} none = scalar-function-err nil
function-err-src {T = T₁ ⇒ T₂} p = function-err p
function-err-src {T = none} (scalar-scalar number () p)
function-err-src {T = none} (scalar-function-ok ())
function-err-src {T = any} none = any
function-err-src {T = boolean} p = scalar-function-err boolean
function-err-src {T = number} p = scalar-function-err number
function-err-src {T = string} p = scalar-function-err string
function-err-src {T = T₁ ∪ T₂} (left p) = left (function-err-src p)
function-err-src {T = T₁ ∪ T₂} (right p) = right (function-err-src p)
function-err-src {T = T₁ ∩ T₂} (p₁ , p₂) = function-err-src p₁ , function-err-src p₂

¬function-err-src : ∀ {T t} → (Language (src T) t) → ¬Language T (function-err t)
¬function-err-src {T = nil} (scalar ())
¬function-err-src {T = T₁ ⇒ T₂} p = function-err p
¬function-err-src {T = none} any = none
¬function-err-src {T = any} (scalar ())
¬function-err-src {T = boolean} (scalar ())
¬function-err-src {T = number} (scalar ())
¬function-err-src {T = string} (scalar ())
¬function-err-src {T = T₁ ∪ T₂} (p₁ , p₂) = (¬function-err-src p₁ , ¬function-err-src p₂)
¬function-err-src {T = T₁ ∩ T₂} (left p) = left (¬function-err-src p)
¬function-err-src {T = T₁ ∩ T₂} (right p) = right (¬function-err-src p)

src-¬function-err : ∀ {T t} → Language T (function-err t) → (¬Language (src T) t)
src-¬function-err {T = nil} p = none
src-¬function-err {T = T₁ ⇒ T₂} (function-err p) = p
src-¬function-err {T = none} (scalar-function-err ())
src-¬function-err {T = any} p = none
src-¬function-err {T = boolean} p = none
src-¬function-err {T = number} p = none
src-¬function-err {T = string} p = none
src-¬function-err {T = T₁ ∪ T₂} (left p) = left (src-¬function-err p)
src-¬function-err {T = T₁ ∪ T₂} (right p) = right (src-¬function-err p)
src-¬function-err {T = T₁ ∩ T₂} (p₁ , p₂) = (src-¬function-err p₁ , src-¬function-err p₂)

src-¬scalar : ∀ {S T t} (s : Scalar S) → Language T (scalar s) → (¬Language (src T) t)
src-¬scalar number (scalar number) = none
src-¬scalar boolean (scalar boolean) = none
src-¬scalar string (scalar string) = none
src-¬scalar nil (scalar nil) = none
src-¬scalar s (left p) = left (src-¬scalar s p)
src-¬scalar s (right p) = right (src-¬scalar s p)
src-¬scalar s (p₁ , p₂) = (src-¬scalar s p₁ , src-¬scalar s p₂)
src-¬scalar s any = none

src-any-≮: : ∀ {T U} → (T ≮: src U) → (U ≮: (T ⇒ any))
src-any-≮: (witness t p q) = witness (function-err t) (function-err-src q) (¬function-err-src p)

any-src-≮: : ∀ {S T U} → (U ≮: S) → (T ≮: (U ⇒ any)) → (U ≮: src T)
any-src-≮: (witness t x x₁) (witness (scalar s) p (function-scalar s)) = witness t x (src-¬scalar s p)
any-src-≮: r (witness (function-ok (scalar s)) p (function-ok (scalar-scalar s () q)))
any-src-≮: r (witness (function-ok (function-ok _)) p (function-ok (scalar-function-ok ())))
any-src-≮: r (witness (function-err t) p (function-err q)) = witness t q (src-¬function-err p)

-- Properties of any and none
any-≮: : ∀ {T U} → (T ≮: U) → (any ≮: U)
any-≮: (witness t p q) = witness t any q

none-≮: : ∀ {T U} → (T ≮: U) → (T ≮: none)
none-≮: (witness t p q) = witness t p none

any-≮:-none : (any ≮: none)
any-≮:-none = witness (scalar nil) any none

function-≮:-none : ∀ {T U} → ((T ⇒ U) ≮: none)
function-≮:-none = witness function function none
