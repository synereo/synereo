FROM ubuntu:precise

ENV SBT_VERSION 0.13.12

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0x219BD9C9

RUN echo 'deb http://repos.azulsystems.com/debian stable main' > /etc/apt/sources.list.d/zulu.list

RUN apt-get -qqy update

RUN apt-get -qqy install curl git mongodb rabbitmq-server supervisor zulu-8

RUN mkdir -p /tmp/mongo

# https://docs.mongodb.com/manual/tutorial/manage-journaling/#journaling-avoid-preallocation-lag
RUN bash -c '(sleep 180; kill $$) & exec mongod --port 10000 --dbpath /tmp/mongo --journal --smallfiles' || true

RUN mkdir -p /data/db
RUN mv /tmp/mongo/journal /data/db
RUN chown -R mongodb:nogroup /data/db
RUN chmod 0600 /data/db/journal/*

WORKDIR /usr/local
RUN curl -sL https://dl.bintray.com/sbt/native-packages/sbt/$SBT_VERSION/sbt-$SBT_VERSION.tgz | tar xz
RUN ln -s /usr/local/sbt/bin/sbt /usr/local/bin/sbt

RUN curl -sL https://dl.eff.org/certbot-auto > /usr/local/bin/certbot-auto
RUN chmod a+x /usr/local/bin/certbot-auto

RUN mkdir -p /opt/synereo

RUN adduser --disabled-password --gecos '' synereo && \
    passwd -l synereo && \
    chown -R synereo:synereo /opt/synereo

USER synereo

WORKDIR /tmp
RUN sbt update
RUN rm -rf target

WORKDIR /home/synereo

VOLUME /data/db

EXPOSE 5672 27017
