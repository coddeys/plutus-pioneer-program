#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=$(jq -r '.ciNodeSocket' plutip/local-cluster-info.json)
cardano-cli query tip --mainnet
