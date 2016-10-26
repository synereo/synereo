#! /usr/bin/env bash

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823

echo "deb https://dl.bintray.com/sbt/debian /" > /etc/apt/sources.list.d/sbt.list

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0x219BD9C9

echo 'deb http://repos.azulsystems.com/debian stable main' > /etc/apt/sources.list.d/zulu.list

apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D

echo 'deb https://apt.dockerproject.org/repo ubuntu-precise main' > /etc/apt/sources.list.d/docker.list

apt-get -qqy update

apt-get -qqy install git mongodb rabbitmq-server zulu-8 sbt \
        linux-image-generic-lts-trusty linux-headers-generic-lts-trusty \
        apt-transport-https ca-certificates docker-engine socat \
        curl tmux vim

usermod -a -G docker vagrant

git clone https://github.com/synereo/synereo.git

chown -R vagrant\: synereo
