{-# OPTIONS --rewriting #-}

module Properties.Subtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src; tgt)
open import Properties.Contradiction using (CONTRADICTION; ¬)
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

unknown-≮:-scalar : ∀ {U} → (Scalar U) → (unknown ≮: U)
unknown-≮:-scalar s = witness (function-ok (scalar s)) unknown (scalar-function-ok s)

scalar-≮:-never : ∀ {U} → (Scalar U) → (U ≮: never)
scalar-≮:-never s = witness (scalar s) (scalar s) never

scalar-≢-impl-≮: : ∀ {T U} → (Scalar T) → (Scalar U) → (T ≢ U) → (T ≮: U)
scalar-≢-impl-≮: s₁ s₂ p = witness (scalar s₁) (scalar s₁) (scalar-scalar s₁ s₂ p)

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

skalar-function-ok : ∀ {t} → (¬Language skalar (function-ok t))
skalar-function-ok = (scalar-function-ok number , (scalar-function-ok string , (scalar-function-ok nil , scalar-function-ok boolean)))

skalar-scalar : ∀ {T} (s : Scalar T) → (Language skalar (scalar s))
skalar-scalar number = left (scalar number)
skalar-scalar boolean = right (right (right (scalar boolean)))
skalar-scalar string = right (left (scalar string))
skalar-scalar nil = right (right (left (scalar nil)))

tgt-never-≮: : ∀ {T U} → (tgt T ≮: U) → (T ≮: (skalar ∪ (never ⇒ U)))
tgt-never-≮: (witness t p q) = witness (function-ok t) (tgt-function-ok p) (skalar-function-ok , function-ok q)

never-tgt-≮: : ∀ {T U} → (T ≮: (skalar ∪ (never ⇒ U))) → (tgt T ≮: U)
never-tgt-≮: (witness (scalar s) p (q₁ , q₂)) = CONTRADICTION (≮:-refl (witness (scalar s) (skalar-scalar s) q₁))
never-tgt-≮: (witness function p (q₁ , scalar-function ()))
never-tgt-≮: (witness (function-ok t) p (q₁ , function-ok q₂)) = witness t (function-ok-tgt p) q₂
never-tgt-≮: (witness (function-err (scalar s)) p (q₁ , function-err (scalar ())))

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

src-¬scalar : ∀ {S T t} (s : Scalar S) → Language T (scalar s) → (¬Language (src T) t)
src-¬scalar number (scalar number) = never
src-¬scalar boolean (scalar boolean) = never
src-¬scalar string (scalar string) = never
src-¬scalar nil (scalar nil) = never
src-¬scalar s (left p) = left (src-¬scalar s p)
src-¬scalar s (right p) = right (src-¬scalar s p)
src-¬scalar s (p₁ , p₂) = (src-¬scalar s p₁ , src-¬scalar s p₂)
src-¬scalar s unknown = never

src-unknown-≮: : ∀ {T U} → (T ≮: src U) → (U ≮: (T ⇒ unknown))
src-unknown-≮: (witness t p q) = witness (function-err t) (function-err-src q) (¬function-err-src p)

unknown-src-≮: : ∀ {S T U} → (U ≮: S) → (T ≮: (U ⇒ unknown)) → (U ≮: src T)
unknown-src-≮: (witness t x x₁) (witness (scalar s) p (function-scalar s)) = witness t x (src-¬scalar s p)
unknown-src-≮: r (witness (function-ok (scalar s)) p (function-ok (scalar-scalar s () q)))
unknown-src-≮: r (witness (function-ok (function-ok _)) p (function-ok (scalar-function-ok ())))
unknown-src-≮: r (witness (function-err t) p (function-err q)) = witness t q (src-¬function-err p)

-- Properties of unknown and never
unknown-≮: : ∀ {T U} → (T ≮: U) → (unknown ≮: U)
unknown-≮: (witness t p q) = witness t unknown q

never-≮: : ∀ {T U} → (T ≮: U) → (T ≮: never)
never-≮: (witness t p q) = witness t p never

unknown-≮:-never : (unknown ≮: never)
unknown-≮:-never = witness (scalar nil) unknown never

function-≮:-never : ∀ {T U} → ((T ⇒ U) ≮: never)
function-≮:-never = witness function function never

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

set-theoretic-if : ∀ {S₁ T₁ S₂ T₂} →

  -- This is the "if" part of being a set-theoretic model
  (Language (S₁ ⇒ T₁) ⊆ Language (S₂ ⇒ T₂)) →
  (∀ Q → Q ⊆ Comp((Language S₁) ⊗ Comp(Language T₁)) → Q ⊆ Comp((Language S₂) ⊗ Comp(Language T₂)))

set-theoretic-if {S₁} {T₁} {S₂} {T₂} p Q q (t , u) Qtu (S₂t , ¬T₂u) = q (t , u) Qtu (S₁t , ¬T₁u) where

  S₁t : Language S₁ t
  S₁t with dec-language S₁ t
  S₁t | Left ¬S₁t with p (function-err t) (function-err ¬S₁t)
  S₁t | Left ¬S₁t | function-err ¬S₂t = CONTRADICTION (language-comp t ¬S₂t S₂t)
  S₁t | Right r = r

  ¬T₁u : ¬(Language T₁ u)
  ¬T₁u T₁u with p (function-ok u) (function-ok T₁u)
  ¬T₁u T₁u | function-ok T₂u = ¬T₂u T₂u

not-quite-set-theoretic-only-if : ∀ {S₁ T₁ S₂ T₂} →

  -- We don't quite have that this is a set-theoretic model
  -- it's only true when Language T₁ and ¬Language T₂ t₂ are inhabited
  -- in particular it's not true when T₁ is never, or T₂ is unknown.
  ∀ s₂ t₂ → Language S₂ s₂ → ¬Language T₂ t₂ →

  -- This is the "only if" part of being a set-theoretic model
  (∀ Q → Q ⊆ Comp((Language S₁) ⊗ Comp(Language T₁)) → Q ⊆ Comp((Language S₂) ⊗ Comp(Language T₂))) →
  (Language (S₁ ⇒ T₁) ⊆ Language (S₂ ⇒ T₂))

not-quite-set-theoretic-only-if {S₁} {T₁} {S₂} {T₂} s₂ t₂ S₂s₂ ¬T₂t₂ p = r where

  Q : (Tree × Tree) → Set
  Q (t , u) = Either (¬Language S₁ t) (Language T₁ u)

  q : Q ⊆ Comp((Language S₁) ⊗ Comp(Language T₁))
  q (t , u) (Left ¬S₁t) (S₁t , ¬T₁u) = language-comp t ¬S₁t S₁t
  q (t , u) (Right T₂u) (S₁t , ¬T₁u) = ¬T₁u T₂u

  r : Language (S₁ ⇒ T₁) ⊆ Language (S₂ ⇒ T₂)
  r function function = function
  r (function-err t) (function-err ¬S₁t) with dec-language S₂ t
  r (function-err t) (function-err ¬S₁t) | Left ¬S₂t = function-err ¬S₂t
  r (function-err t) (function-err ¬S₁t) | Right S₂t = CONTRADICTION (p Q q (t , t₂) (Left ¬S₁t) (S₂t , language-comp t₂ ¬T₂t₂))
  r (function-ok t) (function-ok T₁t) with dec-language T₂ t
  r (function-ok t) (function-ok T₁t) | Left ¬T₂t = CONTRADICTION (p Q q (s₂ , t) (Right T₁t) (S₂s₂ ,  language-comp t ¬T₂t))
  r (function-ok t) (function-ok T₁t) | Right T₂t = function-ok T₂t

-- A counterexample when the argument type is empty.

set-theoretic-counterexample-one : (∀ Q → Q ⊆ Comp((Language never) ⊗ Comp(Language number)) → Q ⊆ Comp((Language never) ⊗ Comp(Language string)))
set-theoretic-counterexample-one Q q ((scalar s) , u) Qtu (scalar () , p)
set-theoretic-counterexample-one Q q ((function-err t) , u) Qtu (scalar-function-err () , p)

set-theoretic-counterexample-two : (never ⇒ number) ≮: (never ⇒ string)
set-theoretic-counterexample-two = witness
  (function-ok (scalar number)) (function-ok (scalar number))
  (function-ok (scalar-scalar number string (λ ())))

-- At some point we may deal with overloaded function resolution, which should fix this problem...
-- The reason why this is connected to overloaded functions is that currently we have that the type of
-- f(x) is (tgt T) where f:T. Really we should have the type depend on the type of x, that is use (tgt T U),
-- where U is the type of x. In particular (tgt (S => T) (U & V)) should be the same as (tgt ((S&U) => T) V)
-- and tgt(never => T) should be unknown. For example
--
-- tgt((number => string) & (string => bool))(number)
-- is tgt(number => string)(number) & tgt(string => bool)(number)
-- is tgt(number => string)(number) & tgt(string => bool)(number&unknown)
-- is tgt(number => string)(number) & tgt(string&number => bool)(unknown)
-- is tgt(number => string)(number) & tgt(never => bool)(unknown)
-- is string & unknown
-- is string
--
-- there's some discussion of this in the Gentle Introduction paper.
