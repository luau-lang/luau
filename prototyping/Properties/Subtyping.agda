{-# OPTIONS --rewriting #-}

module Properties.Subtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src; tgt)
open import Properties.Contradiction using (CONTRADICTION; ¬)
open import Properties.DecSubtyping using (language-comp; dec-language; tgt-function-ok; function-ok-tgt; function-err-src; ¬function-err-src; src-¬function-err)
open import Properties.Equality using (_≢_)
open import Properties.Functions using (_∘_)
open import Properties.Product using (_×_; _,_)

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
