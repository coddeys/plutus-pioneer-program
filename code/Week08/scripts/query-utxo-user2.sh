#!/bin/sh
export CARDANO_NODE_SOCKET_PATH=$(jq -r '.ciNodeSocket' plutip/local-cluster-info.json)
wallets=$(jq -r '.ciWallets | .[1]' plutip/local-cluster-info.json)
address=$(echo $wallets | jq -r .[1] | tr -d '"' )
cardano-cli query utxo --mainnet --address $address
