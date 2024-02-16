## cerra.io P2P lending smart contract - mainnet
[Enter dApp here](https://app.cerra.io/)

## Building cerra.io P2P lending smart contract

- In nix shell run `cabal run cli compile`
- Run `./build.sh` and see the result in `plutus` folder

## Smart contract validation
When interacting with the smart contract through Cerra APP, address and policy ids you are interacting with must match the ones provided in this repository.

When contract address and policy ids are manually built with compile command, it must correspond to the ones provided in this repository.

## Tweag audit

P2P lending smart contracts have been audited by Tweag.

Audit finished on 2024-02-15.

tweag-audit-2024 folder contains:
* test case suite
* instructions how to run it
* locked Cerra P2P lending smart contract version, which was audited

tweag-report-2024 folder contains:
* Audit report

## Exploit remediation 
- all exploits, reported by Tweag, have been fixed
- fixes can be found in the last commit of the repository
- currently deployed smart contract version contains all the fixes, and is being used by [Cerra P2P lending app](https://app.cerra.io/)
