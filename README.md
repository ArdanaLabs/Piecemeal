# Piecemeal

Building a contract piecemeal, and assessing how each change impacts the network cost (script size).

This repo is based off the slimmed down DanaSwap repo (not `plutus-starter`, as it uses a suboptimal Nix setup for Haskell).

Run `cabal run` in nix-shell.

## Sizes

Code size in bytes for each validator [defined here](https://github.com/ArdanaLabs/Piecemeal/tree/main/src/Piecemeal):

| File              | Validator                                 | Script size in bytes |
| ----------------- | ----------------------------------------- | -------------------: |
| Empty.hs          | ()                                        |                   14 |
|                   | Empty.hs, with `ScriptContext`[^ectx]     |                 2065 |
| TypedValidator.hs | (), Typed Validator                       |                 2498 |
| Hello.hs          | `data`, `newtype` (w/ `TypedValidator`)   |                 2575 |
| -                 | `data`, `newtype` (sans `TypedValidator`) |                 2224 |

## Resources

- [How to analyse the cost and size of Plutus scripts](https://marlowe-playground-staging.plutus.aws.iohkdev.io/doc/plutus/howtos/analysing-scripts.html)

[^ectx]: Determined by applying this patch on to `Empty.hs`:

        ```diff
        {-# INLINEABLE mkValidator #-}
        mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
        -mkValidator _ _ _ = ()
        +mkValidator d r ctx =
        +  mkValidator' d r (PlutusTx.unsafeFromBuiltinData ctx)
        +
        +{-# INLINEABLE mkValidator' #-}
        +mkValidator' :: BuiltinData -> BuiltinData -> ScriptContext -> ()
        +mkValidator' _ _ _ = ()
        ```
