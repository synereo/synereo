#! /usr/bin/env bash

set -e

echo '\n' | sudo add-apt-repository ppa:bitcoin/bitcoin

sudo apt-get -qq update

sudo apt-get -qqy install autoconf libtool libdb4.8++-dev libboost-all-dev libssl-dev pkg-config memcached jq

if [ -f "$HOME/omnicore-$OMNICORE_VERSION/autogen.sh" ]; then
    echo "Using cached Omni build"
    pushd "$HOME/omnicore-$OMNICORE_VERSION"
    sudo make install
    popd
else
    pushd $HOME
    curl -sL "https://bintray.com/artifact/download/omni/OmniBinaries/omnicore-$OMNICORE_VERSION.tar.gz" | tar xz
    popd
    pushd "$HOME/omnicore-$OMNICORE_VERSION"
    ./autogen.sh
    ./configure --without-gui
    make
    sudo make install
    popd
fi
