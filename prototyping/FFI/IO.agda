module FFI.IO where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)

open import FFI.Data.HaskellString using (HaskellString)

infixl 1 _>>=_

postulate
  return : ∀ {a} {A : Set a} → A → IO A
  _>>=_  : ∀ {a b} {A : Set a} {B : Set b} → IO A → (A → IO B) → IO B

{-# COMPILE GHC return = \_ _ -> return    #-}
{-# COMPILE GHC _>>=_  = \_ _ _ _ -> (>>=) #-}

postulate getContents : IO HaskellString
{-# COMPILE GHC getContents = getContents #-}

postulate putStrLn : HaskellString → IO ⊤
{-# COMPILE GHC putStrLn = putStrLn #-}

