#!/bin/bash

assets=~/code/cardano/plutus-pioneer-program/code/Week02/assets
keypath=~/code/cardano/plutus-pioneer-program/keys
name="$1"
collateral="$2"
txin="$3"

pp="$assets/protocol-parameters.json"
body="$assets/redeemer-two-true.txbody"
tx="$assets/redeemer-two-true.tx"

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/redeemer-two-true.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/two-true.json" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$body"

# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"
