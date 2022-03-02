{-# OPTIONS --rewriting #-}

module PrettyPrinter where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Int using (pos)
open import Agda.Builtin.Unit using (⊤)

open import FFI.IO using (getContents; putStrLn; _>>=_; _>>_)
open import FFI.Data.Aeson using (Value; eitherDecode)
open import FFI.Data.Either using (Left; Right)
open import FFI.Data.String using (String; _++_)
open import FFI.Data.Text.Encoding using (encodeUtf8)
open import FFI.System.Exit using (exitWith; ExitFailure)

open import Luau.Syntax using (Block)
open import Luau.Syntax.FromJSON using (blockFromJSON)
open import Luau.Syntax.ToString using (blockToString)

runBlock : ∀ {a} → Block a → IO ⊤
runBlock block = putStrLn (blockToString block)

runJSON : Value → IO ⊤
runJSON value with blockFromJSON(value)
runJSON value | (Left err) = putStrLn ("Luau error: " ++ err) >> exitWith (ExitFailure (pos 1))
runJSON value | (Right block) = runBlock block

runString : String → IO ⊤
runString txt with eitherDecode (encodeUtf8 txt)
runString txt | (Left err) = putStrLn ("JSON error: " ++ err) >> exitWith (ExitFailure (pos 1))
runString txt | (Right value) = runJSON value

main : IO ⊤
main = getContents >>= runString
