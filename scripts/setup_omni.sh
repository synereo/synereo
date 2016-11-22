#! /usr/bin/env bash

set -e

if [ ! $(type -P "jq") ]; then
    echo "Please install jq" 1>&2
    exit 1
fi

if [ "$(uname)" == "Darwin" ]; then
    mkdir -p "$HOME/Library/Application Support/Bitcoin"
    BITCOIN_CONF="$HOME/Library/Application Support/Bitcoin/bitcoin.conf"
else
    mkdir -p "$HOME/.bitcoin"
    BITCOIN_CONF="$HOME/.bitcoin/bitcoin.conf"
fi

cp ./gloseval/bitcoin.conf "$BITCOIN_CONF"

omnicored -daemon -rpcport=18332 &>/dev/null

# omnicore-cli -rpcwait setgenerate true 101 &>/dev/null

# COIN_ADDRESS=$(omnicore-cli getnewaddress)

# omnicore-cli sendtoaddress "$COIN_ADDRESS" 25 &>/dev/null

# omnicore-cli setgenerate true 1 &>/dev/null

# omnicore-cli omni_sendissuancefixed "$COIN_ADDRESS" "2" 2 0 "" "" "SynereoCoin" "" "created by for testing AMPs" "1000000000" &>/dev/null

# omnicore-cli setgenerate true 1 &>/dev/null

# PROPERTY_ID=$(omnicore-cli omni_getallbalancesforaddress "$COIN_ADDRESS" | jq '.[0] | .propertyid')

# echo "export PROPERTY_ID=$PROPERTY_ID"
