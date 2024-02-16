# Cerra Lending Audit by Tweag

## Packages

### audit

Contains the auditing code for cerra-lending including a test suite
with regular scenario and attack traces.

### cerra-lending

Contains a subset of cerra's codebase containing the onchain part of
the project. Has been adapted to match cooked-validators
dependencies. Is compiled with a custom workflow accordingly.

## Entering the development environment

Enter the nix environment with `nix develop`. Requires activating nix
flakes experimental feature.

## Building the project

Run `./build.sh`

## Lanching the test suite

Run `cabal run audit`

## Launching traces by hand

- Run `cabal repl audit`
- Import `Audit.Utils`, `Cooked`, `Audit.Traces` and `Audit.Attacks`
- Use `printAndRun traceXXX` to run `traceXXX`
