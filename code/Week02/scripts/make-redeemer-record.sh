#!/bin/sh

assets=~/code/cardano/plutus-pioneer-program/code/Week02/assets
keypath=~/code/cardano/plutus-pioneer-program/keys
name="$1"
txin="$2"
body="$assets/redeemer-record.txbody"
tx="$assets/redeemer-record.tx"

# Build redeemer-record address
cardano-cli address build \
    --payment-script-file "$assets/redeemer-record.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/redeemer-record.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/redeemer-record.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$assets/unit.json" \
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
