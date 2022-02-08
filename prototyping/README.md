# Prototyping Luau

![prototyping workflow](https://github.com/Roblox/luau/actions/workflows/prototyping.yml/badge.svg)

An experimental prototyping system for the Luau type system. This is
intended to allow core language features to be tested quickly, without
having to interact with all the features of production Lua.

## Building

First install Haskell and Agda.

Install dependencies:
```
  cabal update
  cabal install --lib aeson scientific vector
```

Then compile
```
  agda --compile PrettyPrinter.agda
```

and run!
```
  luau-ast Examples/SmokeTest.lua | ./PrettyPrinter
```
