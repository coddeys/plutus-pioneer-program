#!/bin/sh
cardano-node run --config node-config/config.json --topology node-config/topology.json --database-path preview-db --socket-path ipc/node.socket
