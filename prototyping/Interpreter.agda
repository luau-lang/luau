{-# OPTIONS --rewriting #-}

module Interpreter where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Int using (pos)
open import Agda.Builtin.Unit using (⊤)

open import FFI.IO using (getContents; putStrLn; _>>=_; _>>_)
open import FFI.Data.Aeson using (Value; eitherDecode)
open import FFI.Data.Either using (Left; Right)
open import FFI.Data.Maybe using (just; nothing)
open import FFI.Data.String using (String; _++_)
open import FFI.Data.Text.Encoding using (encodeUtf8)
open import FFI.System.Exit using (exitWith; ExitFailure)

open import Luau.StrictMode.ToString using (warningToStringᴮ)
open import Luau.Syntax using (Block; yes; maybe; isAnnotatedᴮ)
open import Luau.Syntax.FromJSON using (blockFromJSON)
open import Luau.Syntax.ToString using (blockToString; valueToString)
open import Luau.Run using (run; return; done; error)
open import Luau.RuntimeError.ToString using (errToStringᴮ)

open import Properties.StrictMode using (wellTypedProgramsDontGoWrong)

runBlock′ : ∀ a → Block a → IO ⊤
runBlock′ a block with run block
runBlock′ a block | return V D = putStrLn ("\nRAN WITH RESULT: " ++ valueToString V)
runBlock′ a block | done D = putStrLn ("\nRAN")
runBlock′ maybe block | error E D = putStrLn ("\nRUNTIME ERROR:\n" ++ errToStringᴮ _ E)
runBlock′ yes block | error E D with wellTypedProgramsDontGoWrong _ block _ D E
runBlock′ yes block | error E D | W = putStrLn ("\nRUNTIME ERROR:\n" ++ errToStringᴮ _ E ++ "\n\nTYPE ERROR:\n" ++ warningToStringᴮ _ W)

runBlock : Block maybe → IO ⊤
runBlock B with isAnnotatedᴮ B
runBlock B | nothing = putStrLn ("UNANNOTATED PROGRAM:\n" ++ blockToString B) >> runBlock′ maybe B
runBlock B | just B′ = putStrLn ("ANNOTATED PROGRAM:\n" ++ blockToString B) >> runBlock′ yes B′

runJSON : Value → IO ⊤
runJSON value with blockFromJSON(value)
runJSON value | (Left err) = putStrLn ("LUAU ERROR: " ++ err) >> exitWith (ExitFailure (pos 1))
runJSON value | (Right block) = runBlock block

runString : String → IO ⊤
runString txt with eitherDecode (encodeUtf8 txt)
runString txt | (Left err) = putStrLn ("JSON ERROR: " ++ err) >> exitWith (ExitFailure (pos 1))
runString txt | (Right value) = runJSON value

main : IO ⊤
main = getContents >>= runString
