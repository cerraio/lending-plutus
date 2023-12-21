## cerra.io P2P lending smart contract - mainnet
[Enter dApp here](https://app.cerra.io/)

## Building cerra.io P2P lending smart contract

- In nix shell run `cabal run cli compile`
- Run `./build.sh` and see the result in `plutus` folder

## Smart contract validation
When interacting with the smart contract through Cerra APP, address and policy ids you are interacting with must match the ones provided in this repository.

When contract address and policy ids are manually built with compile command, it must correspond to the ones provided in this repository.
