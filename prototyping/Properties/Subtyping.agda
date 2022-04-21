{-# OPTIONS --rewriting #-}

module Properties.Subtyping where

open import Agda.Builtin.Equality using (_≡_; refl)
open import FFI.Data.Either using (Either; Left; Right; mapLR; swapLR; cond)
open import FFI.Data.Maybe using (Maybe; just; nothing)
open import Luau.Subtyping using (_<:_; _≮:_; Tree; Language; ¬Language; witness; unknown; never; scalar; function; scalar-function; scalar-function-ok; scalar-function-err; scalar-scalar; function-scalar; function-ok; function-ok₁; function-ok₂; function-err; left; right; _,_)
open import Luau.Type using (Type; Scalar; nil; number; string; boolean; never; unknown; _⇒_; _∪_; _∩_; src; tgt)
open import Properties.Contradiction using (CONTRADICTION; ¬; ⊥)
open import Properties.DecSubtyping using (language-comp; dec-language; function-err-src; ¬function-err-src; src-¬function-err; <:-impl-⊇)
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

-- Properties of intersection

<:-intersect : ∀ {R S T U} → (R <: T) → (S <: U) → ((R ∩ S) <: (T ∩ U))
<:-intersect p q t (r₁ , r₂) = (p t r₁ , q t r₂)

<:-∩-left : ∀ {S T} → (S ∩ T) <: S
<:-∩-left t (p , _) = p

<:-∩-right : ∀ {S T} → (S ∩ T) <: T
<:-∩-right t (_ , p) = p

<:-∩-glb : ∀ {S T U} → (S <: T) → (S <: U) → (S <: (T ∩ U))
<:-∩-glb p q t r = (p t r , q t r)

<:-∩-dist-∪ : ∀ {S T U} → (S ∩ (T ∪ U)) <: ((S ∩ T) ∪ (S ∩ U))
<:-∩-dist-∪ t (p₁ , left p₂) = left (p₁ , p₂)
<:-∩-dist-∪ t (p₁ , right p₂) = right (p₁ , p₂)

-- Properties of functions
<:-function : ∀ {R S T U} → (R <: S) → (T <: U) → (S ⇒ T) <: (R ⇒ U)
<:-function p q function function = function
<:-function p q (function-ok s t) (function-ok₁ r) = function-ok₁ (<:-impl-⊇ p s r)
<:-function p q (function-ok s t) (function-ok₂ r) = function-ok₂ (q t r)
<:-function p q (function-err s) (function-err r) = function-err (<:-impl-⊇ p s r)

<:-function-∩-∪ : ∀ {R S T U} → ((R ⇒ T) ∩ (S ⇒ U)) <: ((R ∪ S) ⇒ (T ∪ U))
<:-function-∩-∪ function (function , function) = function
<:-function-∩-∪ (function-ok s t) (function-ok₁ p₁ , function-ok₁ p₂) = function-ok₁ (p₁ , p₂)
<:-function-∩-∪ (function-ok s t) (p₁ , function-ok₂ p₂) = function-ok₂ (right p₂)
<:-function-∩-∪ (function-ok _ _) (function-ok₂ p₁ , p₂) = function-ok₂ (left p₁)
<:-function-∩-∪ (function-err _) (function-err p₁ , function-err q₂) = function-err (p₁ , q₂)

<:-function-∩ : ∀ {S T U} → ((S ⇒ T) ∩ (S ⇒ U)) <: (S ⇒ (T ∩ U))
<:-function-∩ function (function , function) = function
<:-function-∩ (function-ok s t) (p₁ , function-ok₁ p₂) = function-ok₁ p₂
<:-function-∩ (function-ok s t) (function-ok₁ p₁ , p₂) = function-ok₁ p₁
<:-function-∩ (function-ok s t) (function-ok₂ p₁ , function-ok₂ p₂) = function-ok₂ (p₁ , p₂)
<:-function-∩ (function-err s) (function-err p₁ , function-err p₂) = function-err p₂

-- Properties of scalars
skalar = number ∪ (string ∪ (nil ∪ boolean))

function-≮:-scalar : ∀ {S T U} → (Scalar U) → ((S ⇒ T) ≮: U)
function-≮:-scalar s = witness function function (scalar-function s)

scalar-≮:-function : ∀ {S T U} → (Scalar U) → (U ≮: (S ⇒ T))
scalar-≮:-function s = witness (scalar s) (scalar s) (function-scalar s)

unknown-≮:-scalar : ∀ {U} → (Scalar U) → (unknown ≮: U)
unknown-≮:-scalar s = witness (function) unknown (scalar-function s)

scalar-≮:-never : ∀ {U} → (Scalar U) → (U ≮: never)
scalar-≮:-never s = witness (scalar s) (scalar s) never

scalar-≢-impl-≮: : ∀ {T U} → (Scalar T) → (Scalar U) → (T ≢ U) → (T ≮: U)
scalar-≢-impl-≮: s₁ s₂ p = witness (scalar s₁) (scalar s₁) (scalar-scalar s₁ s₂ p)

skalar-function-ok : ∀ {s t} → (¬Language skalar (function-ok s t))
skalar-function-ok = (scalar-function-ok number , (scalar-function-ok string , (scalar-function-ok nil , scalar-function-ok boolean)))

skalar-scalar : ∀ {T} (s : Scalar T) → (Language skalar (scalar s))
skalar-scalar number = left (scalar number)
skalar-scalar boolean = right (right (right (scalar boolean)))
skalar-scalar string = right (left (scalar string))
skalar-scalar nil = right (right (left (scalar nil)))

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
unknown-src-≮: (witness t p _) (witness (scalar s) q (function-scalar s)) = witness t p (src-¬scalar s q)
unknown-src-≮: (witness _ _ _) (witness function _ (scalar-function ()))
unknown-src-≮: (witness _ _ _) (witness (function-ok _ t) _ (function-ok _ r)) = CONTRADICTION (language-comp t r unknown)
unknown-src-≮: (witness _ _ _) (witness (function-err t) p (function-err q)) = witness t q (src-¬function-err p)

-- Properties of unknown and never
unknown-≮: : ∀ {T U} → (T ≮: U) → (unknown ≮: U)
unknown-≮: (witness t p q) = witness t unknown q

never-≮: : ∀ {T U} → (T ≮: U) → (T ≮: never)
never-≮: (witness t p q) = witness t p never

unknown-≮:-never : (unknown ≮: never)
unknown-≮:-never = witness (scalar nil) unknown never

function-≮:-never : ∀ {T U} → ((T ⇒ U) ≮: never)
function-≮:-never = witness function function never

<:-never : ∀ {T} → (never <: T)
<:-never t (scalar ())
<:-never t (scalar-function-err ())

<:-never-left : ∀ {S T U} → (S <: (T ∪ U)) → (S ≮: T) → (S ∩ U) ≮: never
<:-never-left p (witness t q₁ q₂) with p t q₁
<:-never-left p (witness t q₁ q₂) | left r = CONTRADICTION (language-comp t q₂ r)
<:-never-left p (witness t q₁ q₂) | right r = witness t (q₁ , r) never

<:-never-right : ∀ {S T U} → (S <: (T ∪ U)) → (S ≮: U) → (S ∩ T) ≮: never
<:-never-right p (witness t q₁ q₂) with p t q₁
<:-never-right p (witness t q₁ q₂) | left r = witness t (q₁ , r) never
<:-never-right p (witness t q₁ q₂) | right r = CONTRADICTION (language-comp t q₂ r)

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
  ¬T₁u T₁u | function-ok₁ ¬S₂t = CONTRADICTION (language-comp t ¬S₂t S₂t)
  ¬T₁u T₁u | function-ok₂ T₂u = ¬T₂u T₂u

set-theoretic-if {S₁} {T₁} {S₂} {T₂} p Q q (t , nothing) Qt- (S₂t , _) = q (t , nothing) Qt- (S₁t , λ ()) where

  S₁t : Language S₁ t
  S₁t with dec-language S₁ t
  S₁t | Left ¬S₁t with p (function-err t) (function-err ¬S₁t)
  S₁t | Left ¬S₁t | function-err ¬S₂t = CONTRADICTION (language-comp t ¬S₂t S₂t)
  S₁t | Right r = r

set-theoretic-only-if : ∀ {S₁ T₁ S₂ T₂} →

  -- This is the "only if" part of being a set-theoretic model
  (∀ Q → Q ⊆ Comp((Language S₁) ⊗ Comp(Lift(Language T₁))) → Q ⊆ Comp((Language S₂) ⊗ Comp(Lift(Language T₂)))) →
  (Language (S₁ ⇒ T₁) ⊆ Language (S₂ ⇒ T₂))

set-theoretic-only-if {S₁} {T₁} {S₂} {T₂} p = r where

  Q : (Tree × Maybe Tree) → Set
  Q (t , just u) = Either (¬Language S₁ t) (Language T₁ u)
  Q (t , nothing) = ¬Language S₁ t

  q : Q ⊆ Comp((Language S₁) ⊗ Comp(Lift(Language T₁)))
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
