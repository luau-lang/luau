module Main where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)

open import FFI.IO using (getContents; putStrLn; _>>=_)
open import FFI.Data.Aeson using (Value; eitherDecode)
open import FFI.Data.Either using (Left; Right)
open import FFI.Data.HaskellString using (HaskellString; pack; unpack)
open import FFI.Data.Text.Encoding using (encodeUtf8)

main2 : HaskellString → IO ⊤
main2 txt with eitherDecode (encodeUtf8 (pack txt))
main2 txt | (Left x) = putStrLn x
main2 txt | (Right x) = putStrLn (unpack "OK")

main : IO ⊤
main = getContents >>= main2

