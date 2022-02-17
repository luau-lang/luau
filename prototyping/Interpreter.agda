module Interpreter where

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
open import Luau.Run using (run; return; done; error)
open import Luau.RuntimeError.ToString using (errToStringᴮ)
open import Luau.Value.ToString using (valueToString)

runBlock : ∀ {a} → Block a → IO ⊤
runBlock block with run block
runBlock block | return V D = putStrLn (valueToString V)
runBlock block | done D = putStrLn "nil"
runBlock block | error E D = putStrLn (errToStringᴮ E)

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
