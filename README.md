# Piecemeal

Building a contract piecemeal, and assessing how each change impacts the network cost (script size).

This repo is based off the slimmed down DanaSwap repo (not `plutus-starter`, as it uses a suboptimal Nix setup for Haskell).

Run `cabal run` in nix-shell.

## Sizes

Code size in bytes for each validator [defined here](https://github.com/ArdanaLabs/Piecemeal/tree/main/src/Piecemeal):

| Validator              | Script size in bytes |
| ---------------------- | -------------------: |
| Empty                  |                   14 |
| Empty, Typed Validator |                 2498 |

## Resources

- [How to analyse the cost and size of Plutus scripts](https://marlowe-playground-staging.plutus.aws.iohkdev.io/doc/plutus/howtos/analysing-scripts.html)