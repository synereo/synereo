#! /usr/bin/env bash

set -e

OMNICORE_VERSION="0.0.11.1-rel"

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823

echo "deb https://dl.bintray.com/sbt/debian /" > /etc/apt/sources.list.d/sbt.list

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0x219BD9C9

echo 'deb http://repos.azulsystems.com/debian stable main' > /etc/apt/sources.list.d/zulu.list

apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D

echo 'deb https://apt.dockerproject.org/repo ubuntu-trusty main' > /etc/apt/sources.list.d/docker.list

add-apt-repository ppa:bitcoin/bitcoin

apt-get -qqy update

apt-get -qqy install git mongodb rabbitmq-server zulu-8 sbt \
        apt-transport-https ca-certificates docker-engine socat \
        build-essential autoconf libtool libdb4.8++-dev libboost-all-dev libssl-dev pkg-config \
        curl tmux vim jq \

mkdir -p /usr/local/src
pushd /usr/local/src
curl -sL "https://bintray.com/artifact/download/omni/OmniBinaries/omnicore-$OMNICORE_VERSION.tar.gz" | tar xz
cd "omnicore-$OMNICORE_VERSION"
./autogen.sh
./configure
make
make install
popd

usermod -a -G docker vagrant

git clone https://github.com/synereo/synereo.git

chown -R vagrant\: synereo
