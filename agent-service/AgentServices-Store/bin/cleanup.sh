#! /bin/sh
sudo ~/work/src/devtools/mongo/mongodb-linux-x86_64-2.4.5/bin/mongo records --eval "db.dropDatabase()"; \
sudo ~/work/src/devtools/rabbitmq/rabbitmq_server-2.6.1/sbin/rabbitmqctl stop_app; \
sudo ~/work/src/devtools/rabbitmq/rabbitmq_server-2.6.1/sbin/rabbitmqctl reset; \
sudo ~/work/src/devtools/rabbitmq/rabbitmq_server-2.6.1/sbin/rabbitmqctl stop; \
sudo sleep 2; \
mv KVDBLogs.log* history; \
sudo ~/work/src/devtools/rabbitmq/rabbitmq_server-2.6.1/sbin/rabbitmq-server &


