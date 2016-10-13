FROM ubuntu:precise

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
RUN find /data/db/journal -type f -exec chmod 0600 {} \;

RUN curl -sL https://dl.eff.org/certbot-auto > /usr/local/bin/certbot-auto
RUN chmod a+x /usr/local/bin/certbot-auto

RUN adduser --disabled-password --gecos '' synereo && passwd -l synereo

USER synereo

WORKDIR /home/synereo

VOLUME /data/db

EXPOSE 5672 27017
