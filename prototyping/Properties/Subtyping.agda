{-# OPTIONS --rewriting #-}

module Properties.Subtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-function-tgt; scalar-scalar; function-scalar; function-ok; function-ok₁; function-ok₂; function-err; function-tgt; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; skalar)
open import Properties.Contradiction using (CONTRADICTION; ¬; ⊥)
open import Properties.Equality using (_≢_)
open import Properties.Functions using (_∘_)
open import Properties.Product using (_×_; _,_)

-- Language membership is decidable
dec-language : ∀ T t → Either (¬Language T t) (Language T t)
dec-language nil (scalar number) = Left (scalar-scalar number nil (λ ()))
dec-language nil (scalar boolean) = Left (scalar-scalar boolean nil (λ ()))
dec-language nil (scalar string) = Left (scalar-scalar string nil (λ ()))
dec-language nil (scalar nil) = Right (scalar nil)
dec-language nil function = Left (scalar-function nil)
dec-language nil (function-ok s t) = Left (scalar-function-ok nil)
dec-language nil (function-err t) = Left (scalar-function-err nil)
dec-language boolean (scalar number) = Left (scalar-scalar number boolean (λ ()))
dec-language boolean (scalar boolean) = Right (scalar boolean)
dec-language boolean (scalar string) = Left (scalar-scalar string boolean (λ ()))
dec-language boolean (scalar nil) = Left (scalar-scalar nil boolean (λ ()))
dec-language boolean function = Left (scalar-function boolean)
dec-language boolean (function-ok s t) = Left (scalar-function-ok boolean)
dec-language boolean (function-err t) = Left (scalar-function-err boolean)
dec-language number (scalar number) = Right (scalar number)
dec-language number (scalar boolean) = Left (scalar-scalar boolean number (λ ()))
dec-language number (scalar string) = Left (scalar-scalar string number (λ ()))
dec-language number (scalar nil) = Left (scalar-scalar nil number (λ ()))
dec-language number function = Left (scalar-function number)
dec-language number (function-ok s t) = Left (scalar-function-ok number)
dec-language number (function-err t) = Left (scalar-function-err number)
dec-language string (scalar number) = Left (scalar-scalar number string (λ ()))
dec-language string (scalar boolean) = Left (scalar-scalar boolean string (λ ()))
dec-language string (scalar string) = Right (scalar string)
dec-language string (scalar nil) = Left (scalar-scalar nil string (λ ()))
dec-language string function = Left (scalar-function string)
dec-language string (function-ok s t) = Left (scalar-function-ok string)
dec-language string (function-err t) = Left (scalar-function-err string)
dec-language (T₁ ⇒ T₂) (scalar s) = Left (function-scalar s)
dec-language (T₁ ⇒ T₂) function = Right function
dec-language (T₁ ⇒ T₂) (function-ok s t) = cond (Right ∘ function-ok₁) (λ p → mapLR (function-ok p) function-ok₂ (dec-language T₂ t)) (dec-language T₁ s)
dec-language (T₁ ⇒ T₂) (function-err t) = mapLR function-err function-err (swapLR (dec-language T₁ t))
dec-language never t = Left never
dec-language unknown t = Right unknown
dec-language (T₁ ∪ T₂) t = cond (λ p → cond (Left ∘ _,_ p) (Right ∘ right) (dec-language T₂ t)) (Right ∘ left) (dec-language T₁ t)
dec-language (T₁ ∩ T₂) t = cond (Left ∘ left) (λ p → cond (Left ∘ right) (Right ∘ _,_ p) (dec-language T₂ t)) (dec-language T₁ t)
dec-language nil (function-tgt t) = Left (scalar-function-tgt nil)
dec-language (T₁ ⇒ T₂) (function-tgt t) = mapLR function-tgt function-tgt (dec-language T₂ t)
dec-language boolean (function-tgt t) = Left (scalar-function-tgt boolean)
dec-language number (function-tgt t) = Left (scalar-function-tgt number)
dec-language string (function-tgt t) = Left (scalar-function-tgt string)

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
language-comp (function-ok s t) (scalar-function-ok ()) (function-ok₁ p)
language-comp (function-ok s t) (function-ok p₁ p₂) (function-ok₁ q) = language-comp s q p₁
language-comp (function-ok s t) (function-ok p₁ p₂) (function-ok₂ q) = language-comp t p₂ q
language-comp (function-err t) (function-err p) (function-err q) = language-comp t q p 
language-comp (function-tgt t) (scalar-function-tgt ()) (function-tgt q)
language-comp (function-tgt t) (function-tgt p) (function-tgt q) = language-comp t p q

-- ≮: is the complement of <:
¬≮:-impl-<: : ∀ {T U} → ¬(T ≮: U) → (T <: U)
¬≮:-impl-<: {T} {U} p t q with dec-language U t
¬≮:-impl-<: {T} {U} p t q | Left r = CONTRADICTION (p (witness t q r))
¬≮:-impl-<: {T} {U} p t q | Right r = r

<:-impl-¬≮: : ∀ {T U} → (T <: U) → ¬(T ≮: U)
<:-impl-¬≮: p (witness t q r) = language-comp t r (p t q)

<:-impl-⊇ : ∀ {T U} → (T <: U) → ∀ t → ¬Language U t → ¬Language T t
<:-impl-⊇ {T} p t q with dec-language T t
<:-impl-⊇ {_} p t q | Left r = r
<:-impl-⊇ {_} p t q | Right r = CONTRADICTION (language-comp t q (p t r))

-- reflexivity
≮:-refl : ∀ {T} → ¬(T ≮: T)
≮:-refl (witness t p q) = language-comp t q p

<:-refl : ∀ {T} → (T <: T)
<:-refl = ¬≮:-impl-<: ≮:-refl

-- transititivity
≮:-trans-≡ : ∀ {S T U} → (S ≮: T) → (T ≡ U) → (S ≮: U)
≮:-trans-≡ p refl = p

<:-trans-≡ : ∀ {S T U} → (S <: T) → (T ≡ U) → (S <: U)
<:-trans-≡ p refl = p

≡-impl-<: : ∀ {T U} → (T ≡ U) → (T <: U)
≡-impl-<: refl = <:-refl

≡-trans-≮: : ∀ {S T U} → (S ≡ T) → (T ≮: U) → (S ≮: U)
≡-trans-≮: refl p = p

≡-trans-<: : ∀ {S T U} → (S ≡ T) → (T <: U) → (S <: U)
≡-trans-<: refl p = p

≮:-trans : ∀ {S T U} → (S ≮: U) → Either (S ≮: T) (T ≮: U)
≮:-trans {T = T} (witness t p q) = mapLR (witness t p) (λ z → witness t z q) (dec-language T t)

<:-trans : ∀ {S T U} → (S <: T) → (T <: U) → (S <: U)
<:-trans p q t r = q t (p t r)

<:-trans-≮: : ∀ {S T U} → (S <: T) → (S ≮: U) → (T ≮: U)
<:-trans-≮: p (witness t q r) = witness t (p t q) r

≮:-trans-<: : ∀ {S T U} → (S ≮: U) → (T <: U) → (S ≮: T)
≮:-trans-<: (witness t p q) r = witness t p (<:-impl-⊇ r t q)

-- Properties of union

<:-union : ∀ {R S T U} → (R <: T) → (S <: U) → ((R ∪ S) <: (T ∪ U))
<:-union p q t (left r) = left (p t r)
<:-union p q t (right r) = right (q t r)

<:-∪-left : ∀ {S T} → S <: (S ∪ T)
<:-∪-left t p = left p

<:-∪-right : ∀ {S T} → T <: (S ∪ T)
<:-∪-right t p = right p

<:-∪-lub : ∀ {S T U} → (S <: U) → (T <: U) → ((S ∪ T) <: U)
<:-∪-lub p q t (left r) = p t r
<:-∪-lub p q t (right r) = q t r

<:-∪-symm : ∀ {T U} → (T ∪ U) <: (U ∪ T)
<:-∪-symm t (left p) = right p
<:-∪-symm t (right p) = left p

<:-∪-assocl : ∀ {S T U} → (S ∪ (T ∪ U)) <: ((S ∪ T) ∪ U)
<:-∪-assocl t (left p) = left (left p)
<:-∪-assocl t (right (left p)) = left (right p)
<:-∪-assocl t (right (right p)) = right p

<:-∪-assocr : ∀ {S T U} → ((S ∪ T) ∪ U) <: (S ∪ (T ∪ U))
<:-∪-assocr t (left (left p)) = left p
<:-∪-assocr t (left (right p)) = right (left p)
<:-∪-assocr t (right p) = right (right p)

≮:-∪-left : ∀ {S T U} → (S ≮: U) → ((S ∪ T) ≮: U)
≮:-∪-left (witness t p q) = witness t (left p) q

≮:-∪-right : ∀ {S T U} → (T ≮: U) → ((S ∪ T) ≮: U)
≮:-∪-right (witness t p q) = witness t (right p) q

≮:-left-∪ : ∀ {S T U} → (S ≮: (T ∪ U)) → (S ≮: T)
≮:-left-∪ (witness t p (q₁ , q₂)) = witness t p q₁

≮:-right-∪ : ∀ {S T U} → (S ≮: (T ∪ U)) → (S ≮: U)
≮:-right-∪ (witness t p (q₁ , q₂)) = witness t p q₂

-- Properties of intersection

<:-intersect : ∀ {R S T U} → (R <: T) → (S <: U) → ((R ∩ S) <: (T ∩ U))
<:-intersect p q t (r₁ , r₂) = (p t r₁ , q t r₂)

<:-∩-left : ∀ {S T} → (S ∩ T) <: S
<:-∩-left t (p , _) = p

<:-∩-right : ∀ {S T} → (S ∩ T) <: T
<:-∩-right t (_ , p) = p

<:-∩-glb : ∀ {S T U} → (S <: T) → (S <: U) → (S <: (T ∩ U))
<:-∩-glb p q t r = (p t r , q t r)

<:-∩-symm : ∀ {T U} → (T ∩ U) <: (U ∩ T)
<:-∩-symm t (p₁ , p₂) = (p₂ , p₁)

<:-∩-assocl : ∀ {S T U} → (S ∩ (T ∩ U)) <: ((S ∩ T) ∩ U)
<:-∩-assocl t (p , (p₁ , p₂)) = (p , p₁) , p₂

<:-∩-assocr : ∀ {S T U} → ((S ∩ T) ∩ U) <: (S ∩ (T ∩ U))
<:-∩-assocr t ((p , p₁) , p₂) = p , (p₁ , p₂)

≮:-∩-left : ∀ {S T U} → (S ≮: T) → (S ≮: (T ∩ U))
≮:-∩-left (witness t p q) = witness t p (left q)

≮:-∩-right : ∀ {S T U} → (S ≮: U) → (S ≮: (T ∩ U))
≮:-∩-right (witness t p q) = witness t p (right q)

-- Distribution properties
<:-∩-distl-∪ : ∀ {S T U} → (S ∩ (T ∪ U)) <: ((S ∩ T) ∪ (S ∩ U))
<:-∩-distl-∪ t (p₁ , left p₂) = left (p₁ , p₂)
<:-∩-distl-∪ t (p₁ , right p₂) = right (p₁ , p₂)

∩-distl-∪-<: : ∀ {S T U} → ((S ∩ T) ∪ (S ∩ U)) <: (S ∩ (T ∪ U))
∩-distl-∪-<: t (left (p₁ , p₂)) = (p₁ , left p₂)
∩-distl-∪-<: t (right (p₁ , p₂)) = (p₁ , right p₂)

<:-∩-distr-∪ : ∀ {S T U} → ((S ∪ T) ∩ U) <:  ((S ∩ U) ∪ (T ∩ U))
<:-∩-distr-∪ t (left p₁ , p₂) = left (p₁ , p₂)
<:-∩-distr-∪ t (right p₁ , p₂) = right (p₁ , p₂)

∩-distr-∪-<: : ∀ {S T U} → ((S ∩ U) ∪ (T ∩ U)) <: ((S ∪ T) ∩ U)
∩-distr-∪-<: t (left (p₁ , p₂)) = (left p₁ , p₂)
∩-distr-∪-<: t (right (p₁ , p₂)) = (right p₁ , p₂)

<:-∪-distl-∩ : ∀ {S T U} → (S ∪ (T ∩ U)) <: ((S ∪ T) ∩ (S ∪ U))
<:-∪-distl-∩ t (left p) = (left p , left p)
<:-∪-distl-∩ t (right (p₁ , p₂)) = (right p₁ , right p₂)

∪-distl-∩-<: : ∀ {S T U} → ((S ∪ T) ∩ (S ∪ U)) <: (S ∪ (T ∩ U))
∪-distl-∩-<: t (left p₁ , p₂) = left p₁
∪-distl-∩-<: t (right p₁ , left p₂) = left p₂
∪-distl-∩-<: t (right p₁ , right p₂) = right (p₁ , p₂)

<:-∪-distr-∩ : ∀ {S T U} → ((S ∩ T) ∪ U) <: ((S ∪ U) ∩ (T ∪ U))
<:-∪-distr-∩ t (left (p₁ , p₂)) = left p₁ , left p₂
<:-∪-distr-∩ t (right p) = (right p , right p)

∪-distr-∩-<: : ∀ {S T U} → ((S ∪ U) ∩ (T ∪ U)) <: ((S ∩ T) ∪ U)
∪-distr-∩-<: t (left p₁ , left p₂) = left (p₁ , p₂)
∪-distr-∩-<: t (left p₁ , right p₂) = right p₂
∪-distr-∩-<: t (right p₁ , p₂) = right p₁

∩-<:-∪ : ∀ {S T} → (S ∩ T) <: (S ∪ T)
∩-<:-∪ t (p , _) = left p

-- Properties of functions
<:-function : ∀ {R S T U} → (R <: S) → (T <: U) → (S ⇒ T) <: (R ⇒ U)
<:-function p q function function = function
<:-function p q (function-ok s t) (function-ok₁ r) = function-ok₁ (<:-impl-⊇ p s r)
<:-function p q (function-ok s t) (function-ok₂ r) = function-ok₂ (q t r)
<:-function p q (function-err s) (function-err r) = function-err (<:-impl-⊇ p s r)
<:-function p q (function-tgt t) (function-tgt r) = function-tgt (q t r)

<:-function-∩-∩ : ∀ {R S T U} → ((R ⇒ T) ∩ (S ⇒ U)) <: ((R ∩ S) ⇒ (T ∩ U))
<:-function-∩-∩ function (function , function) = function
<:-function-∩-∩ (function-ok s t) (function-ok₁ p , q) = function-ok₁ (left p)
<:-function-∩-∩ (function-ok s t) (function-ok₂ p , function-ok₁ q) = function-ok₁ (right q)
<:-function-∩-∩ (function-ok s t) (function-ok₂ p , function-ok₂ q) = function-ok₂ (p , q)
<:-function-∩-∩ (function-err s) (function-err p , q) = function-err (left p)
<:-function-∩-∩ (function-tgt s) (function-tgt p , function-tgt q) = function-tgt (p , q)

<:-function-∩-∪ : ∀ {R S T U} → ((R ⇒ T) ∩ (S ⇒ U)) <: ((R ∪ S) ⇒ (T ∪ U))
<:-function-∩-∪ function (function , function) = function
<:-function-∩-∪ (function-ok s t) (function-ok₁ p₁ , function-ok₁ p₂) = function-ok₁ (p₁ , p₂)
<:-function-∩-∪ (function-ok s t) (p₁ , function-ok₂ p₂) = function-ok₂ (right p₂)
<:-function-∩-∪ (function-ok s t) (function-ok₂ p₁ , p₂) = function-ok₂ (left p₁)
<:-function-∩-∪ (function-err s) (function-err p₁ , function-err q₂) = function-err (p₁ , q₂)
<:-function-∩-∪ (function-tgt t) (function-tgt p , q) = function-tgt (left p)

<:-function-∩ : ∀ {S T U} → ((S ⇒ T) ∩ (S ⇒ U)) <: (S ⇒ (T ∩ U))
<:-function-∩ function (function , function) = function
<:-function-∩ (function-ok s t) (p₁ , function-ok₁ p₂) = function-ok₁ p₂
<:-function-∩ (function-ok s t) (function-ok₁ p₁ , p₂) = function-ok₁ p₁
<:-function-∩ (function-ok s t) (function-ok₂ p₁ , function-ok₂ p₂) = function-ok₂ (p₁ , p₂)
<:-function-∩ (function-err s) (function-err p₁ , function-err p₂) = function-err p₂
<:-function-∩ (function-tgt t) (function-tgt p₁ , function-tgt p₂) = function-tgt (p₁ , p₂)

<:-function-∪ : ∀ {R S T U} → ((R ⇒ S) ∪ (T ⇒ U)) <: ((R ∩ T) ⇒ (S ∪ U))
<:-function-∪ function (left function) = function
<:-function-∪ (function-ok s t) (left (function-ok₁ p)) = function-ok₁ (left p)
<:-function-∪ (function-ok s t) (left (function-ok₂ p)) = function-ok₂ (left p)
<:-function-∪ (function-err s) (left (function-err p)) = function-err (left p)
<:-function-∪ (scalar s) (left (scalar ()))
<:-function-∪ function (right function) = function
<:-function-∪ (function-ok s t) (right (function-ok₁ p)) = function-ok₁ (right p)
<:-function-∪ (function-ok s t) (right (function-ok₂ p)) = function-ok₂ (right p)
<:-function-∪ (function-err s) (right (function-err x)) = function-err (right x)
<:-function-∪ (scalar s) (right (scalar ()))
<:-function-∪ (function-tgt t) (left (function-tgt p)) = function-tgt (left p)
<:-function-∪ (function-tgt t) (right (function-tgt p)) = function-tgt (right p)

<:-function-∪-∩ : ∀ {R S T U} → ((R ∩ S) ⇒ (T ∪ U)) <: ((R ⇒ T) ∪ (S ⇒ U))
<:-function-∪-∩ function function = left function
<:-function-∪-∩ (function-ok s t) (function-ok₁ (left p)) = left (function-ok₁ p)
<:-function-∪-∩ (function-ok s t) (function-ok₂ (left p)) = left (function-ok₂ p)
<:-function-∪-∩ (function-ok s t) (function-ok₁ (right p)) = right (function-ok₁ p)
<:-function-∪-∩ (function-ok s t) (function-ok₂ (right p)) = right (function-ok₂ p)
<:-function-∪-∩ (function-err s) (function-err (left p)) = left (function-err p)
<:-function-∪-∩ (function-err s) (function-err (right p)) = right (function-err p)
<:-function-∪-∩ (function-tgt t) (function-tgt (left p)) = left (function-tgt p)
<:-function-∪-∩ (function-tgt t) (function-tgt (right p)) = right (function-tgt p)

<:-function-left : ∀ {R S T U} → (S ⇒ T) <: (R ⇒ U) → (R <: S)
<:-function-left {R} {S} p s Rs with dec-language S s
<:-function-left p s Rs | Right Ss = Ss
<:-function-left p s Rs | Left ¬Ss with p (function-err s) (function-err ¬Ss)
<:-function-left p s Rs | Left ¬Ss | function-err ¬Rs = CONTRADICTION (language-comp s ¬Rs Rs)

<:-function-right : ∀ {R S T U} → (S ⇒ T) <: (R ⇒ U) → (T <: U)
<:-function-right p t Tt with p (function-tgt t) (function-tgt Tt)
<:-function-right p t Tt | function-tgt St = St

≮:-function-left : ∀ {R S T U} → (R ≮: S) → (S ⇒ T) ≮: (R ⇒ U)
≮:-function-left (witness t p q) = witness (function-err t) (function-err q) (function-err p)

≮:-function-right : ∀ {R S T U} → (T ≮: U) → (S ⇒ T) ≮: (R ⇒ U)
≮:-function-right (witness t p q) = witness (function-tgt t) (function-tgt p) (function-tgt q)

-- Properties of scalars
skalar-function-ok : ∀ {s t} → (¬Language skalar (function-ok s t))
skalar-function-ok = (scalar-function-ok number , (scalar-function-ok string , (scalar-function-ok nil , scalar-function-ok boolean)))

scalar-<: : ∀ {S T} → (s : Scalar S) → Language T (scalar s) → (S <: T)
scalar-<: number p (scalar number) (scalar number) = p
scalar-<: boolean p (scalar boolean) (scalar boolean) = p
scalar-<: string p (scalar string) (scalar string) = p
scalar-<: nil p (scalar nil) (scalar nil) = p

scalar-∩-function-<:-never : ∀ {S T U} → (Scalar S) → ((T ⇒ U) ∩ S) <: never
scalar-∩-function-<:-never number .(scalar number) (() , scalar number)
scalar-∩-function-<:-never boolean .(scalar boolean) (() , scalar boolean)
scalar-∩-function-<:-never string .(scalar string) (() , scalar string)
scalar-∩-function-<:-never nil .(scalar nil) (() , scalar nil)

function-≮:-scalar : ∀ {S T U} → (Scalar U) → ((S ⇒ T) ≮: U)
function-≮:-scalar s = witness function function (scalar-function s)

scalar-≮:-function : ∀ {S T U} → (Scalar U) → (U ≮: (S ⇒ T))
scalar-≮:-function s = witness (scalar s) (scalar s) (function-scalar s)

unknown-≮:-scalar : ∀ {U} → (Scalar U) → (unknown ≮: U)
unknown-≮:-scalar s = witness function unknown (scalar-function s)

scalar-≮:-never : ∀ {U} → (Scalar U) → (U ≮: never)
scalar-≮:-never s = witness (scalar s) (scalar s) never

scalar-≢-impl-≮: : ∀ {T U} → (Scalar T) → (Scalar U) → (T ≢ U) → (T ≮: U)
scalar-≢-impl-≮: s₁ s₂ p = witness (scalar s₁) (scalar s₁) (scalar-scalar s₁ s₂ p)

scalar-≢-∩-<:-never : ∀ {T U V} → (Scalar T) → (Scalar U) → (T ≢ U) → (T ∩ U) <: V
scalar-≢-∩-<:-never s t p u (scalar s₁ , scalar s₂) = CONTRADICTION (p refl)

skalar-scalar : ∀ {T} (s : Scalar T) → (Language skalar (scalar s))
skalar-scalar number = left (scalar number)
skalar-scalar boolean = right (right (right (scalar boolean)))
skalar-scalar string = right (left (scalar string))
skalar-scalar nil = right (right (left (scalar nil)))

-- Properties of unknown and never
unknown-≮: : ∀ {T U} → (T ≮: U) → (unknown ≮: U)
unknown-≮: (witness t p q) = witness t unknown q

never-≮: : ∀ {T U} → (T ≮: U) → (T ≮: never)
never-≮: (witness t p q) = witness t p never

unknown-≮:-never : (unknown ≮: never)
unknown-≮:-never = witness (scalar nil) unknown never

unknown-≮:-function : ∀ {S T} → (unknown ≮: (S ⇒ T))
unknown-≮:-function = witness (scalar nil) unknown (function-scalar nil)

function-≮:-never : ∀ {T U} → ((T ⇒ U) ≮: never)
function-≮:-never = witness function function never

<:-never : ∀ {T} → (never <: T)
<:-never t (scalar ())

≮:-never-left : ∀ {S T U} → (S <: (T ∪ U)) → (S ≮: T) → (S ∩ U) ≮: never
≮:-never-left p (witness t q₁ q₂) with p t q₁
≮:-never-left p (witness t q₁ q₂) | left r = CONTRADICTION (language-comp t q₂ r)
≮:-never-left p (witness t q₁ q₂) | right r = witness t (q₁ , r) never

≮:-never-right : ∀ {S T U} → (S <: (T ∪ U)) → (S ≮: U) → (S ∩ T) ≮: never
≮:-never-right p (witness t q₁ q₂) with p t q₁
≮:-never-right p (witness t q₁ q₂) | left r = witness t (q₁ , r) never
≮:-never-right p (witness t q₁ q₂) | right r = CONTRADICTION (language-comp t q₂ r)

<:-unknown : ∀ {T} → (T <: unknown)
<:-unknown t p = unknown

<:-everything : unknown <: ((never ⇒ unknown) ∪ skalar)
<:-everything (scalar s) p = right (skalar-scalar s)
<:-everything function p = left function
<:-everything (function-ok s t) p = left (function-ok₁ never)
<:-everything (function-err s) p = left (function-err never)
<:-everything (function-tgt t) p = left (function-tgt unknown)

-- A Gentle Introduction To Semantic Subtyping (https://www.cduce.org/papers/gentle.pdf)
-- defines a "set-theoretic" model (sec 2.5)
-- Unfortunately we don't quite have this property, due to uninhabited types,
-- for example (never -> T) is equivalent to (never -> U)
-- when types are interpreted as sets of syntactic values.

_⊆_ : ∀ {A : Set} → (A → Set) → (A → Set) → Set
(P ⊆ Q) = ∀ a → (P a) → (Q a)

_⊗_ : ∀ {A B : Set} → (A → Set) → (B → Set) → ((A × B) → Set)
(P ⊗ Q) (a , b) = (P a) × (Q b)

Comp : ∀ {A : Set} → (A → Set) → (A → Set)
Comp P a = ¬(P a)

Lift : ∀ {A : Set} → (A → Set) → (Maybe A → Set)
Lift P nothing = ⊥
Lift P (just a) = P a

set-theoretic-if : ∀ {S₁ T₁ S₂ T₂} →

  -- This is the "if" part of being a set-theoretic model
  -- though it uses the definition from Frisch's thesis
  -- rather than from the Gentle Introduction. The difference
  -- being the presence of Lift, (written D_Ω in Defn 4.2 of
  -- https://www.cduce.org/papers/frisch_phd.pdf).
  (Language (S₁ ⇒ T₁) ⊆ Language (S₂ ⇒ T₂)) →
  (∀ Q → Q ⊆ Comp((Language S₁) ⊗ Comp(Lift(Language T₁))) → Q ⊆ Comp((Language S₂) ⊗ Comp(Lift(Language T₂))))

set-theoretic-if {S₁} {T₁} {S₂} {T₂} p Q q (t , just u) Qtu (S₂t , ¬T₂u) = q (t , just u) Qtu (S₁t , ¬T₁u) where

  S₁t : Language S₁ t
  S₁t with dec-language S₁ t
  S₁t | Left ¬S₁t with p (function-err t) (function-err ¬S₁t)
  S₁t | Left ¬S₁t | function-err ¬S₂t = CONTRADICTION (language-comp t ¬S₂t S₂t)
  S₁t | Right r = r

  ¬T₁u : ¬(Language T₁ u)
  ¬T₁u T₁u with p (function-ok t u) (function-ok₂ T₁u)
  ¬T₁u T₁u | function-ok₁ ¬S₂t = language-comp t ¬S₂t S₂t
  ¬T₁u T₁u | function-ok₂ T₂u = ¬T₂u T₂u

set-theoretic-if {S₁} {T₁} {S₂} {T₂} p Q q (t , nothing) Qt- (S₂t , _) = q (t , nothing) Qt- (S₁t , λ ()) where

  S₁t : Language S₁ t
  S₁t with dec-language S₁ t
  S₁t | Left ¬S₁t with p (function-err t) (function-err ¬S₁t)
  S₁t | Left ¬S₁t | function-err ¬S₂t = CONTRADICTION (language-comp t ¬S₂t S₂t)
  S₁t | Right r = r

not-quite-set-theoretic-only-if : ∀ {S₁ T₁ S₂ T₂} →

  -- We don't quite have that this is a set-theoretic model
  -- it's only true when Language S₂ is inhabited
  -- in particular it's not true when S₂ is never,
  ∀ s₂ → Language S₂ s₂ →

  -- This is the "only if" part of being a set-theoretic model
  (∀ Q → Q ⊆ Comp((Language S₁) ⊗ Comp(Lift(Language T₁))) → Q ⊆ Comp((Language S₂) ⊗ Comp(Lift(Language T₂)))) →
  (Language (S₁ ⇒ T₁) ⊆ Language (S₂ ⇒ T₂))

not-quite-set-theoretic-only-if {S₁} {T₁} {S₂} {T₂} s₂ S₂s₂ p = r where

  Q : (Tree × Maybe Tree) → Set
  Q (t , just u) = Either (¬Language S₁ t) (Language T₁ u)
  Q (t , nothing) = ¬Language S₁ t
  
  q : Q ⊆ Comp(Language S₁ ⊗ Comp(Lift(Language T₁)))
  q (t , just u) (Left ¬S₁t) (S₁t , ¬T₁u) = language-comp t ¬S₁t S₁t
  q (t , just u) (Right T₂u) (S₁t , ¬T₁u) = ¬T₁u T₂u
  q (t , nothing) ¬S₁t (S₁t , _) = language-comp t ¬S₁t S₁t
  
  r : Language (S₁ ⇒ T₁) ⊆ Language (S₂ ⇒ T₂)
  r function function = function
  r (function-err s) (function-err ¬S₁s) with dec-language S₂ s
  r (function-err s) (function-err ¬S₁s) | Left ¬S₂s = function-err ¬S₂s
  r (function-err s) (function-err ¬S₁s) | Right S₂s = CONTRADICTION (p Q q (s , nothing) ¬S₁s (S₂s , λ ()))
  r (function-ok s t) (function-ok₁ ¬S₁s) with dec-language S₂ s
  r (function-ok s t) (function-ok₁ ¬S₁s) | Left ¬S₂s = function-ok₁ ¬S₂s
  r (function-ok s t) (function-ok₁ ¬S₁s) | Right S₂s = CONTRADICTION (p Q q (s , nothing) ¬S₁s (S₂s , λ ()))
  r (function-ok s t) (function-ok₂ T₁t) with dec-language T₂ t
  r (function-ok s t) (function-ok₂ T₁t) | Left ¬T₂t with dec-language S₂ s
  r (function-ok s t) (function-ok₂ T₁t) | Left ¬T₂t | Left ¬S₂s = function-ok₁ ¬S₂s
  r (function-ok s t) (function-ok₂ T₁t) | Left ¬T₂t | Right S₂s = CONTRADICTION (p Q q (s , just t) (Right T₁t) (S₂s , language-comp t ¬T₂t))
  r (function-ok s t) (function-ok₂ T₁t) | Right T₂t = function-ok₂ T₂t
  r (function-tgt t) (function-tgt T₁t) with dec-language T₂ t
  r (function-tgt t) (function-tgt T₁t) | Left ¬T₂t = CONTRADICTION (p Q q (s₂ , just t) (Right T₁t) (S₂s₂ , language-comp t ¬T₂t))
  r (function-tgt t) (function-tgt T₁t) | Right T₂t = function-tgt T₂t

-- A counterexample when the argument type is empty.

set-theoretic-counterexample-one : (∀ Q → Q ⊆ Comp((Language never) ⊗ Comp(Lift(Language number))) → Q ⊆ Comp((Language never) ⊗ Comp(Lift(Language string))))
set-theoretic-counterexample-one Q q ((scalar s) , u) Qtu (scalar () , p)

set-theoretic-counterexample-two : (never ⇒ number) ≮: (never ⇒ string)
set-theoretic-counterexample-two = witness (function-tgt (scalar number)) (function-tgt (scalar number)) (function-tgt (scalar-scalar number string (λ ())))
