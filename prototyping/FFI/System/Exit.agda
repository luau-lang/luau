module FFI.System.Exit where

open import Agda.Builtin.Int using (Int)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)

data ExitCode : Set where
  ExitSuccess : ExitCode
  ExitFailure : Int → ExitCode

{-# FOREIGN GHC data AgdaExitCode = AgdaExitSuccess | AgdaExitFailure Integer #-}
{-# COMPILE GHC ExitCode = data AgdaExitCode (AgdaExitSuccess | AgdaExitFailure) #-}

{-# FOREIGN GHC import qualified System.Exit #-}

{-# FOREIGN GHC
toExitCode :: AgdaExitCode -> System.Exit.ExitCode
toExitCode AgdaExitSuccess = System.Exit.ExitSuccess
toExitCode (AgdaExitFailure n) = System.Exit.ExitFailure (fromIntegral n)

fromExitCode :: System.Exit.ExitCode -> AgdaExitCode
fromExitCode System.Exit.ExitSuccess = AgdaExitSuccess
fromExitCode (System.Exit.ExitFailure n) = AgdaExitFailure (fromIntegral n)
#-}

postulate
  exitWith : ExitCode → IO ⊤

{-# COMPILE GHC exitWith = System.Exit.exitWith . toExitCode #-}
