#! /bin/sh
sudo ~/work/src/devtools/rabbitmq/rabbitmq_server-2.6.1/sbin/rabbitmqctl stop_app;
sudo ~/work/src/devtools/rabbitmq/rabbitmq_server-2.6.1/sbin/rabbitmqctl reset;
sudo ~/work/src/devtools/rabbitmq/rabbitmq_server-2.6.1/sbin/rabbitmqctl start_app;
sudo mongo records --eval "db.dropDatabase()"

