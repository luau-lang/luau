module FFI.IO where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)

open import FFI.Data.HaskellString using (HaskellString; pack ; unpack)

infixl 1 _>>=_
infixl 1 _>>_

postulate
  return : ∀ {a} {A : Set a} → A → IO A
  _>>=_  : ∀ {a b} {A : Set a} {B : Set b} → IO A → (A → IO B) → IO B
  fmap : ∀ {a b} {A : Set a} {B : Set b} → (A → B) → IO A → IO B

{-# COMPILE GHC return = \_ _ -> return    #-}
{-# COMPILE GHC _>>=_  = \_ _ _ _ -> (>>=) #-}
{-# COMPILE GHC fmap  = \_ _ _ _ -> fmap #-}

postulate getHContents : IO HaskellString
{-# COMPILE GHC getHContents = getContents #-}

postulate putHStrLn : HaskellString → IO ⊤
{-# COMPILE GHC putHStrLn = putStrLn #-}

getContents : IO String
getContents = fmap pack getHContents

putStrLn : String → IO ⊤
putStrLn txt = putHStrLn (unpack txt)

_>>_ : ∀ {a} {A : Set a} → IO ⊤ → IO A → IO A
a >> b = a >>= (λ _ → b )
