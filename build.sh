#!/usr/bin/env bash

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
cd "$here"

export CARDANO_NODE_SOCKET_PATH=$HOME/ipc/node.socket
export MAGIC="--mainnet"

sudo chmod 777 plutus

sudo cardano-cli address build --payment-script-file plutus/lending.plutus $MAGIC --out-file plutus/lending.addr

sudo cardano-cli transaction policyid --script-file plutus/factory.plutus > plutus/factory
sudo cardano-cli transaction policyid --script-file plutus/borrower.plutus > plutus/borrower
sudo cardano-cli transaction policyid --script-file plutus/lender.plutus > plutus/lender

sudo chmod 777 plutus/*