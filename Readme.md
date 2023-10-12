## WIP cerra.io P2P lending smart contract
[Enter dApp here](https://app.cerra.io/)

## Building cerra.io P2P lending smart contract

- In nix shell run `cabal run cli compile`
- Run `./build.sh` and see the result in `plutus` folder

### !Important
Smart contract is in development and constantly changing.

Once compiled and built, smart contract address will not correspond to the one currently used in the testnet.

Smart contract releases (deployments) to testnet are planned once every 2 weeks.

Once released to production (mainnet) smart contract address and NFT policies (in /plutus folder) MUST ALWAYS correspond to the ones used in [dApp](https://app.cerra.io/)
